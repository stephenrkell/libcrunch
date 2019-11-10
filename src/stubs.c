#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include "libcrunch_private.h"

_Bool __libcrunch_is_initialized = 1;
extern _Bool our_init_flag __attribute__((visibility("hidden"),alias("__libcrunch_is_initialized")));

struct __liballocs_memrange_cache __libcrunch_fake_bounds_cache; // all zeroes

int __libcrunch_debug_level __attribute__((weak));

unsigned long int __libcrunch_begun = 0;
unsigned long int __libcrunch_failed = 0;
unsigned long int __libcrunch_succeeded = 0;
unsigned long int __libcrunch_aborted_typestr = 0;
unsigned long int __libcrunch_is_a_hit_cache = 0;
unsigned long int __libcrunch_created_invalid_pointer = 0;
unsigned long int __libcrunch_fetch_bounds_called = 0;
unsigned long int __libcrunch_fetch_bounds_missed_cache = 0;
unsigned long int __libcrunch_primary_secondary_transitions = 0;
unsigned long int __libcrunch_fault_handler_fixups = 0;
unsigned long int __libcrunch_ptr_derivations = 0;
unsigned long int __libcrunch_ptr_derefs = 0;
unsigned long int __libcrunch_ptr_stores = 0;

void **__libcrunch_bounds_bases_region_00;
void **__libcrunch_bounds_bases_region_2a;
void **__libcrunch_bounds_bases_region_7a;
unsigned long *__libcrunch_bounds_sizes_region_00;
unsigned long *__libcrunch_bounds_sizes_region_2a;
unsigned long *__libcrunch_bounds_sizes_region_7a;

static void print_exit_summary(void)
{
	if (__libcrunch_ptr_derefs == 0
		&& !getenv("LIBCRUNCH_ALWAYS_PRINT_EXIT_SUMMARY")) return;
	
	fprintf(stderr, "======================================================\n");
	fprintf(stderr, "libcrunch stub runtime summary: \n");
	fprintf(stderr, "------------------------------------------------------\n");
	fprintf(stderr, "pointer dereferences:                      % 11ld\n", __libcrunch_ptr_derefs);
	fprintf(stderr, "   of which stored shadowed pointer values:% 11ld\n", __libcrunch_ptr_stores);
	fprintf(stderr, "pointer derivations instrumented:          % 11ld\n", __libcrunch_ptr_derivations);
	fprintf(stderr, "------------------------------------------------------\n");
	fprintf(stderr, "out-of-bounds pointers created:            % 11ld\n", __libcrunch_created_invalid_pointer);
	fprintf(stderr, "accesses trapped and emulated:             % 11ld\n", 0ul /* FIXME */);
	fprintf(stderr, "calls to __fetch_bounds:                   % 11ld\n", __libcrunch_fetch_bounds_called /* FIXME: remove */);
	fprintf(stderr, "   of which missed cache:                  % 11ld\n", __libcrunch_fetch_bounds_missed_cache);
	fprintf(stderr, "calls requiring secondary checks           % 11ld\n", __libcrunch_primary_secondary_transitions);
	fprintf(stderr, "trap-pointer fixups in fault handler       % 11ld\n", __libcrunch_fault_handler_fixups);
	fprintf(stderr, "======================================================\n");
}

void __liballocs_systrap_init(void);
static void init(void) __attribute__((constructor));
static void init(void)
{
	/* It's critical that we detect whether we're being overridden,
	 * and skip this init if so. The preload code in liballocs wants to
	 * initialise systrap *only* after it has scanned /proc/self/maps, 
	 * so that it can accurately track mmappings. To test for overriddenness,
	 * we use a hidden alias for something that will be overridden by our
	 * overrider, here the init flag. */
	//if (&__libcrunch_is_initialized == &our_init_flag) __liballocs_systrap_init();
	/* HACK: the above is broken by LTO on gcc 5.x onwards (see bug 78407)
	 * so instead use the is-really-loaded trick. */
	_Bool libcrunch_is_loaded = (&__libcrunch_really_loaded);
	if (!libcrunch_is_loaded)
	{
		__liballocs_systrap_init();
		atexit(print_exit_summary);
	}
}

void __libcrunch_scan_lazy_typenames(void *blah) {}

int __libcrunch_check_init(void)
{
	return 0;
}

int __libcrunch_global_init(void)
{
	return 0;
}

int __is_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __like_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __loosely_like_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __named_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __check_args_internal(const void *obj, int nargs, ...)
{
	return 1;
}

int __is_a_function_refining_internal(const void *obj, const void *r)
{
	return 1;
}
int __is_a_pointer_of_degree_internal(const void *obj, int d)
{
	return 1;
}
int __can_hold_pointer_internal(const void *obj, const void *target)
{
	return 1;
}

__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *derived, const struct uniqtype *t)
{
	return __libcrunch_max_bounds(ptr);
}

void __libcrunch_bounds_error(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds)
{
}
void __libcrunch_soft_deref_error_at(const void *codeptr, __libcrunch_bounds_t bounds, const void *addr)
{
}

struct uniqtype;
struct __libcrunch_bounds_s;
typedef struct __libcrunch_bounds_s __libcrunch_bounds_t;

__libcrunch_bounds_t __fetch_bounds_ool(const void *ptr, const void *derived_ptr, struct uniqtype *t)
{
	return __libcrunch_max_bounds(ptr);
}

__libcrunch_bounds_t __fetch_bounds_ool_via_dladdr(const void *ptr, const void *derived_ptr, struct uniqtype *t)
{
	return __libcrunch_max_bounds(ptr);
}

void (__attribute__((nonnull(1))) __store_pointer_nonlocal_via_voidptrptr)(const void **dest, const void *srcval, __libcrunch_bounds_t val_bounds, struct uniqtype *static_guessed_srcval_pointee_type)
{
	/* do nothing */
}

void __ensure_bounds_in_cache(unsigned long ptrval, __libcrunch_bounds_t ptr_bounds, struct uniqtype *t)
{}
