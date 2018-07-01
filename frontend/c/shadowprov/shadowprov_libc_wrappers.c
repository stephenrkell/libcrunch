#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>
#include <err.h>
#include <time.h>
#include <assert.h>
#include "shadowprov_helpers.h"
#define RELF_DEFINE_STRUCTURES
#include "relf.h" /* for the auxv stuff */

#define NULL_PROV NULL_SHADOW
#define SPECIAL_PROV SPECIAL_SHADOW
#define NULL_SHADOW ((char)0)
#define SPECIAL_SHADOW ((char)255) /* FIXME: do I need this? */

#define MANGLE(sym) __wrap_ ## sym
#define REAL(sym) __real_ ## sym
#define DECLARE(ret, name, ...) \
    ret REAL(name)(__VA_ARGS__); \
	ret MANGLE(name)(__VA_ARGS__)
#define BEGIN(name) _Bool caller_is_inst = __tweak_argument_shadow_cookie(MANGLE( name ))
#define RETURN_SHADOWABLE(t, v, rvals...) \
		__push_result_shadow_manifest( \
			caller_is_inst, (__shadowed_value_t)(v), ## rvals); \
		return (t)(v)
#define RETURN_NULL RETURN_SHADOWABLE(void*, NULL, NULL_PROV, 0)
#define RETURN_SHADOWABLE_WITH_ARGSHADOW(t, v, off, argname) \
		__push_local_result_shadow( \
			caller_is_inst, \
			__peek_argument_shadow(caller_is_inst, (off), (__shadowed_value_t) argname, "argument " #argname ) \
		); \
		return (t)(v)
#define RETURN_FRESH_NULL_POINTER \
		__push_result_shadow_explicit(caller_is_inst, NULL, NULL_PROV); \
		return NULL
#define GENERATE_FROM_BASE(v) \
	(__make_shadow((unsigned long) v, 0))
#define RETURN_SHADOWABLE_FRESH(t, v) \
		__push_result_shadow_manifest( \
			caller_is_inst, (__shadowed_value_t)(v), (unsigned long)(v), 0); \
		return (t)(v)
#define RETURN_SHADOWABLE_EXPLICIT(t, v, c) \
		__push_result_shadow_explicit( \
			caller_is_inst, (__shadowed_value_t)(v), (c)); \
		return (t)(v)

/* Some common wrapper pattens:
 * 
 * - provide expressions for the return shadow;
 * - propagate an argument shadow to the return shadow; 
 */

DECLARE(FILE*, tmpfile, void)
{
	BEGIN(tmpfile);
	FILE *ret_shadowable = REAL(tmpfile)();
	RETURN_SHADOWABLE_FRESH(FILE*, ret_shadowable);
}

DECLARE(DIR*, fdopendir, int fd)
{
	BEGIN(fdopendir);
	void* ret_shadowable = REAL(fdopendir)(fd);
	RETURN_SHADOWABLE_FRESH(DIR*, ret_shadowable);
}

DECLARE(char*, strchr, const char *s, int c)
{
	BEGIN(strchr);
	char *ret_shadowable = REAL(strchr)(s, c);
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, ret_shadowable, 0, s);
	}
	else
	{
		RETURN_NULL;
	}
}

#if defined(__linux__)

DECLARE(int*, __errno_location, void)
{
	BEGIN(__errno_location);
	static char shadow;
	int *ret = REAL(__errno_location)();
	if (!shadow) shadow = GENERATE_FROM_BASE(ret).byte;
	RETURN_SHADOWABLE_EXPLICIT(int*, ret, shadow);
}

#endif

DECLARE(short const **, __ctype_b_loc, void)
{
	BEGIN(__ctype_b_loc);
	short const** ret_shadowable = REAL(__ctype_b_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FROM_BASE(__liballocs_get_alloc_base(ret_shadowable)).byte;
	RETURN_SHADOWABLE_EXPLICIT(short const **, ret_shadowable, shadow);
}

DECLARE(int **, __ctype_toupper_loc, void)
{
	BEGIN(__ctype_toupper_loc);
	int ** ret_shadowable = REAL(__ctype_toupper_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FROM_BASE(__liballocs_get_alloc_base(ret_shadowable)).byte;
	RETURN_SHADOWABLE_EXPLICIT(int**, ret_shadowable, shadow);
}

DECLARE(int **, __ctype_tolower_loc, void)
{
	BEGIN(__ctype_tolower_loc);
	int ** ret_shadowable = REAL(__ctype_tolower_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FROM_BASE(__liballocs_get_alloc_base(ret_shadowable)).byte;
	RETURN_SHADOWABLE_EXPLICIT(int**, ret_shadowable, shadow);
}

DECLARE(FILE*, fopen, const char *fname, const char *mode)
{
	BEGIN(fopen);
	FILE *ret_shadowable = REAL(fopen)(fname, mode);
	RETURN_SHADOWABLE_FRESH(FILE*, ret_shadowable);
}

DECLARE(char*, strtok, char *str, const char *delim)
{
	BEGIN(strtok);
	char *ret_shadowable = REAL(strtok)(str, delim);
	/* ret_shadowable points to a private libc-side buffer. */
	RETURN_SHADOWABLE_FRESH(char*, ret_shadowable);
}

DECLARE(char*, fgets, char *s, int size, FILE *stream)
{
	BEGIN(fgets);
	char *ret_shadowable = REAL(fgets)(s, size, stream);
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, ret_shadowable, 0, s);
	}
	else
	{
		RETURN_NULL;
	}
}

DECLARE(char *, strstr, const char *haystack, const char *needle)
{
	BEGIN(strstr);
	char *ret_shadowable = REAL(strstr)(haystack, needle);
	RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, ret_shadowable, 0, haystack);
}

DECLARE(char*, strdup, const char *s)
{
	BEGIN(strdup);
	char *ret_shadowable = REAL(strdup)(s);
	RETURN_SHADOWABLE_FRESH(char*, ret_shadowable);
}

DECLARE(char*, getenv, const char *s)
{
	BEGIN(getenv);
	char *ret_shadowable = REAL(getenv)(s);
	if (ret_shadowable)
	{
		/* HACK */
		RETURN_SHADOWABLE_FRESH(char*, ret_shadowable); 
	}
	RETURN_NULL;
	
}

extern __thread void *__current_allocsite;
DECLARE(void*, malloc, size_t s)
{
	BEGIN(malloc);
	_Bool we_set_allocsite = 0;
	if (!__current_allocsite)
	{
		we_set_allocsite = 1;
		__current_allocsite = __builtin_return_address(0);
	}
	void *ret_shadowable = REAL(malloc)(s);
	if (we_set_allocsite) __current_allocsite = NULL;
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}
DECLARE(void*, calloc, size_t nmemb, size_t size)
{
	BEGIN(calloc);
	_Bool we_set_allocsite = 0;
	if (!__current_allocsite)
	{
		we_set_allocsite = 1;
		__current_allocsite = __builtin_return_address(0);
	}
	void *ret_shadowable = REAL(calloc)(nmemb, size);
	if (we_set_allocsite) __current_allocsite = NULL;
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}
DECLARE(void*, realloc, void *p, size_t size)
{
	BEGIN(realloc);
	_Bool we_set_allocsite = 0;
	if (!__current_allocsite)
	{
		we_set_allocsite = 1;
		__current_allocsite = __builtin_return_address(0);
	}
	void *ret_shadowable = REAL(realloc)(p, size);
	if (we_set_allocsite) __current_allocsite = NULL;
	if (ret_shadowable == p)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(
			void*, ret_shadowable, 0, p);
	}
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}
DECLARE(void*, dlsym, void *handle, const char *symbol)
{
	BEGIN(dlsym);
	void *ret = REAL(dlsym)(handle, symbol);
	if (ret)
	{
		/* Assume we get the base address of some object. */
		RETURN_SHADOWABLE_FRESH(void*, ret);
	}
	RETURN_NULL;
}
DECLARE(char*, strrchr, const char *s, int c)
{
	BEGIN(strrchr);
	char *ret = REAL(strrchr)(s, c);
	if (ret)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, ret, 0, s);
	}
	RETURN_NULL;
}
DECLARE(struct tm*, localtime, const time_t *timep)
{
	BEGIN(localtime);
	static char shadow;
	struct tm *ret = REAL(localtime)(timep);
	if (ret)
	{
		if (!shadow) shadow = GENERATE_FROM_BASE(ret).byte;
		RETURN_SHADOWABLE_EXPLICIT(struct tm*, ret, shadow);
	}
	RETURN_NULL;
}
DECLARE(void*, dlopen, const char *filename, int flag)
{
	BEGIN(dlopen);
	void *ret = REAL(dlopen)(filename, flag);
	if (ret)
	{
		RETURN_SHADOWABLE_EXPLICIT(void *, ret, GENERATE_FROM_BASE(ret).byte);
	}
	RETURN_NULL;
}
DECLARE(char*, strcat, char *dest, char *src)
{
	BEGIN(strcat);
	char *ret = REAL(strcat)(dest, src);
	RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, dest, 0, dest);
}
DECLARE(char*, getcwd, char *buf, size_t size)
{
	BEGIN(getcwd);
	char *ret = REAL(getcwd)(buf, size);
	if (ret)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(char *, buf, 0, buf);
	}
	RETURN_NULL;
}
DECLARE(FILE*, fdopen, int fd, const char *mode)
{
	BEGIN(fdopen);
	FILE *ret_shadowable = REAL(fdopen)(fd, mode);
	RETURN_SHADOWABLE_FRESH(FILE*, ret_shadowable);
}
DECLARE(void*, memset, void *s, int c, size_t n)
{
	BEGIN(memset);
	void *ret = REAL(memset)(s, c, n);
	RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, s, 0, s);
}
DECLARE(char *, strcpy, char *dest, const char *src)
{
	BEGIN(strcpy);
	char *ret = REAL(strcpy)(dest, src);
	RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, dest, 0, dest);
}

// we don't need to init the libc/auxv shadow space here -- shadow.c does it
// OH, but it initialises things with bounds -- we want provenances.
#define STORED_PTR(ptr) warnx("Set up stored shadowable at %p: %02x", &ptr, __make_shadow((unsigned long) ptr, 0)); __store_shadow_nonlocal(&ptr, (unsigned long) ptr, __make_shadow((unsigned long) ptr, 0), (void*)0)
#define STORE_IT2(x, y) __store_shadow_nonlocal(&x, (unsigned long) y, __make_shadow((unsigned long) y, 0), (void*)0)
#define STORE_IT_AT(place, val) __store_shadow_nonlocal(place, (unsigned long) val, __make_shadow((unsigned long) val, 0), (void*)0)
extern int main(int, char**) __attribute__((weak));
static void init_shadow_entries(void) __attribute__((constructor));
static void init_shadow_entries(void)
{
	// from gdb:
	// printf "%lx\n", *(unsigned char*) ((void*(*)(void*)) __libcrunch_ool_base_stored_addr)( & shadowable )
	/* It's not just about wrapping functions. Initialise the globals.
	 * FIXME: not sure why SoftBound doesn't do this. */
	Elf64_auxv_t *auxv_array_start = get_auxv((const char **) environ, environ[0]);
	if (!auxv_array_start) return;

	struct auxv_limits lims = get_auxv_limits(auxv_array_start);

	for (const char **argvi = lims.argv_vector_start; argvi != lims.argv_vector_terminator; ++argvi)
	{
		/* We're pointing at a stored pointer. */
		STORE_IT2(*argvi, lims.asciiz_start);
	}
	for (const char **envi = lims.env_vector_start; envi != lims.env_vector_terminator; ++envi)
	{
		/* We're pointing at a stored pointer. */
		STORE_IT2(*envi, lims.asciiz_start);
	}
	STORE_IT2(environ, lims.env_vector_start); // in case environ is not init'd yet?
	

	STORED_PTR(stdin);
	STORED_PTR(stdout);
	STORED_PTR(stderr);
		
	/* HACK: some glibc-specific stuff. */
	STORE_IT_AT((void**) __ctype_b_loc(), __liballocs_get_alloc_base(*(void**) __ctype_b_loc()));
	STORE_IT_AT((void**) __ctype_toupper_loc(), __liballocs_get_alloc_base(*(void**) __ctype_toupper_loc()));
	STORE_IT_AT((void**) __ctype_tolower_loc(), __liballocs_get_alloc_base(*(void**) __ctype_tolower_loc()));
	/* END glibc-specific */
	
	struct link_map *exe_handle = get_exe_handle();
	void *main_addr;
	if (&main) main_addr = &main;
	else main_addr = fake_dlsym(exe_handle, "main");
	if (!main_addr || main_addr == (void*) -1) warnx("Could not get address of main; initial cookie will be invalid");
	// FIXME: also look at static alloc records
	/* Leave shadows for argc and argv there on the shadow stack for main() to pick up. */
	__push_argument_shadow_manifest((unsigned long) lims.argv_vector_start,
		(unsigned long) lims.argv_vector_start, 0);
	/* This is for argc */
	__push_argument_shadow_manifest((unsigned long) 0, (unsigned long) 0, 0);
	__push_argument_shadow_cookie(main_addr, "main");
}
