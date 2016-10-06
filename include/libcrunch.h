#ifndef LIBCRUNCH_H_
#define LIBCRUNCH_H_

#include "liballocs.h"

#if !defined(NO_PURE) && !defined(PURE)
#define PURE __attribute__((pure))
#elif !defined(PURE)
#define PURE
#endif

extern int __libcrunch_debug_level;
extern _Bool __libcrunch_is_initialized __attribute__((weak));
#ifdef LIBCRUNCH_EXTENDED_COUNTS
extern unsigned long __libcrunch_aborted_init __attribute__((weak));
extern unsigned long __libcrunch_trivially_succeeded __attribute__((weak));
extern unsigned long __libcrunch_checked_pointer_adjustments __attribute__((weak));
#endif
extern unsigned long __libcrunch_begun __attribute__((weak));
extern unsigned long __libcrunch_aborted_typestr __attribute__((weak));
extern unsigned long __libcrunch_failed __attribute__((weak));
extern unsigned long __libcrunch_succeeded __attribute__((weak));
extern unsigned long __libcrunch_is_a_hit_cache __attribute__((weak));
extern unsigned long __libcrunch_created_invalid_pointer __attribute__((weak));
extern unsigned long __libcrunch_fetch_bounds_called __attribute__((weak));
extern unsigned long __libcrunch_fetch_bounds_missed_cache __attribute__((weak));
extern unsigned long __libcrunch_primary_secondary_transitions __attribute__((weak));
extern unsigned long __libcrunch_fault_handler_fixups __attribute__((weak));

int __libcrunch_global_init(void) __attribute__((weak));
// declare as const void *-returning, to simplify trumptr
const void *__libcrunch_typestr_to_uniqtype(const char *typestr) __attribute__((weak));
void *__libcrunch_my_typeobj(void) __attribute__((weak));
int __is_a_internal(const void *obj, const void *uniqtype) __attribute__((weak)) PURE __attribute__((hot));
int __like_a_internal(const void *obj, const void *uniqtype) __attribute__((weak)) PURE __attribute__((hot));;
int __named_a_internal(const void *obj, const void *typestr) __attribute__((weak)) PURE __attribute__((hot));;
int __check_args_internal(const void *obj, int nargs, ...) __attribute__((weak)) PURE __attribute__((hot));;
int __is_a_function_refining_internal(const void *obj, const void *uniqtype) __attribute__((weak)) PURE __attribute__((hot));
int __is_a_function_pointer_of_degree_internal(const void *obj, int d) __attribute__((weak)) PURE __attribute__((hot));
int __can_hold_pointer_internal(const void *target, const void *value) __attribute__((weak)) PURE __attribute__((hot));

/* The main public API to libcrunch is through several small functions 
 * which are *always* inlined. NOTE: repeat these in trumptr.ml so that
 * the instrumentation can add them to code which doesn't include this header. */

// if we're not compiling under CIL, include the things that trumptr would define
#ifndef CIL
#include "libcrunch_cil_inlines.h"
#endif
__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *derived_ptr, struct uniqtype *u);
void * __check_derive_ptr_internal(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t *derivedfrom_bounds, struct uniqtype *t) PURE;

/* our own private assert */
static inline void 
(__attribute__((always_inline,gnu_inline)) __libcrunch_private_assert) (_Bool cond, const char *reason, 
	const char *f, unsigned l, const char *fn)
{
#ifndef NDEBUG
	if (!cond) __assert_fail(reason, f, l, fn);
#endif
}

static inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_ensure_init) (void)
{
	__libcrunch_private_assert(__libcrunch_check_init() == 0, "libcrunch init", 
		__FILE__, __LINE__, __func__);
}

#endif
