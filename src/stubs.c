#define _GNU_SOURCE
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include "libcrunch_private.h"

_Bool __libcrunch_is_initialized = 1;
extern _Bool our_init_flag __attribute__((visibility("hidden"),alias("__libcrunch_is_initialized")));

struct __libcrunch_cache __libcrunch_is_a_cache; // all zeroes
struct __libcrunch_cache __libcrunch_fake_bounds_cache; // all zeroes

unsigned long int __libcrunch_begun = 0;
unsigned long int __libcrunch_failed = 0;
unsigned long int __libcrunch_succeeded = 0;
unsigned long int __libcrunch_aborted_typestr = 0;
unsigned long int __libcrunch_is_a_hit_cache = 0;
unsigned long int __libcrunch_created_invalid_pointer = 0;
unsigned long int __libcrunch_checked_pointer_adjustments = 0;
unsigned long int __libcrunch_fetch_bounds_called = 0;
unsigned long int __libcrunch_fetch_bounds_missed_cache = 0;
unsigned long __libcrunch_primary_secondary_transitions = 0;

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
	if (&__libcrunch_is_initialized == &our_init_flag) __liballocs_systrap_init();
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

__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *derived, struct uniqtype *t)
{
	return __libcrunch_max_bounds(ptr);
}

void __libcrunch_bounds_error(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds)
{
}

struct uniqtype;
struct __libcrunch_bounds_s;
typedef struct __libcrunch_bounds_s __libcrunch_bounds_t;
void * __check_derive_ptr_internal(
		const void *derived, 
		const void *derivedfrom, 
		__libcrunch_bounds_t *opt_derivedfrom_bounds, 
		struct uniqtype *t
)
{
	return (void*) derived;
}

__libcrunch_bounds_t __fetch_bounds_ool(const void *ptr, const void *derived_ptr, struct uniqtype *t)
{
	return __libcrunch_max_bounds(ptr);
}
