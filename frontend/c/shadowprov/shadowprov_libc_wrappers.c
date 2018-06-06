#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>
#include <err.h>
#include "shadowprov_helpers.h"

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
#define GENERATE_FRESH(v) \
	(__make_shadow((unsigned long) v, rand()))
#define RETURN_SHADOWABLE_FRESH(t, v) \
		__push_result_shadow_manifest( \
			caller_is_inst, (__shadowed_value_t)(v), (unsigned long)(v), rand()); \
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
	if (!shadow) shadow = GENERATE_FRESH(ret).byte;
	RETURN_SHADOWABLE_EXPLICIT(int*, ret, shadow);
}

#endif

DECLARE(short const **, __ctype_b_loc, void)
{
	BEGIN(__ctype_b_loc);
	short const** ret_shadowable = REAL(__ctype_b_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FRESH(ret_shadowable).byte;
	RETURN_SHADOWABLE_EXPLICIT(short const **, ret_shadowable, shadow);
}

DECLARE(int **, __ctype_toupper_loc, void)
{
	BEGIN(__ctype_toupper_loc);
	int ** ret_shadowable = REAL(__ctype_toupper_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FRESH(ret_shadowable).byte;
	RETURN_SHADOWABLE_EXPLICIT(int**, ret_shadowable, shadow);
}

DECLARE(int **, __ctype_tolower_loc, void)
{
	BEGIN(__ctype_tolower_loc);
	int ** ret_shadowable = REAL(__ctype_tolower_loc)();
	static char shadow;
	if (!shadow) shadow = GENERATE_FRESH(ret_shadowable).byte;
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
	RETURN_SHADOWABLE_WITH_ARGSHADOW(char*, ret_shadowable, 1, haystack);
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

DECLARE(void*, malloc, size_t s)
{
	BEGIN(malloc);
	void *ret_shadowable = REAL(malloc)(s);
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}
DECLARE(void*, calloc, size_t nmemb, size_t size)
{
	BEGIN(calloc);
	void *ret_shadowable = REAL(calloc)(nmemb, size);
	if (ret_shadowable)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}
DECLARE(void*, realloc, void *p, size_t size)
{
	BEGIN(realloc);
	void *ret_shadowable = REAL(realloc)(p, size);
	if (ret_shadowable == p)
	{
		RETURN_SHADOWABLE_WITH_ARGSHADOW(
			void*, ret_shadowable, 1, p);
	}
	if (p)
	{
		RETURN_SHADOWABLE_FRESH(void*, ret_shadowable);
	}
	RETURN_NULL;
}

// we don't need to init the libc/auxv shadow space here -- shadow.c does it
