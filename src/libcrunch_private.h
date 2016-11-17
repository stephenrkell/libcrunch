#ifndef LIBCRUNCH_PRIVATE_H_
#define LIBCRUNCH_PRIVATE_H_

/* x86_64 only, for now */
#if !defined(__x86_64__) && !defined(X86_64)
#error Unsupported architecture.
#endif

#include "memtable.h"
#include "heap_index.h"
#include "liballocs_private.h"
#include <stdint.h>

#include "libcrunch.h"
#include "systrap.h"

#undef debug_printf /* from liballocs */
extern FILE *crunch_stream_err __attribute__((visibility("hidden")));
#define debug_printf_to(strm, lvl, fmt, ...) do { \
    if ((lvl) <= __libcrunch_debug_level) { \
      fprintf((strm), "%s: " fmt, get_exe_basename(), ## __VA_ARGS__ );  \
    } \
  } while (0)

#define debug_printf(lvl, fmt, ...) debug_printf_to(crunch_stream_err, lvl, fmt, ## __VA_ARGS__ )

/* avoid dependency on libc headers (in this header only) */
void __assert_fail(const char *assertion, 
	const char *file, unsigned int line, const char *function);
void warnx(const char *fmt, ...);
unsigned long malloc_usable_size (void *ptr);

/* counters */
extern unsigned long __libcrunch_begun;
#ifdef LIBCRUNCH_EXTENDED_COUNTS
extern unsigned long __libcrunch_aborted_init;
extern unsigned long __libcrunch_trivially_succeeded_null;
extern unsigned long __libcrunch_checked_pointer_adjustments;
#endif
extern unsigned long __libcrunch_aborted_typestr;
extern unsigned long __libcrunch_lazy_heap_type_assignment;
extern unsigned long __libcrunch_failed_and_suppressed;
extern unsigned long __libcrunch_failed_in_alloc;
extern unsigned long __libcrunch_succeeded;
extern unsigned long __libcrunch_is_a_hit_cache;
extern unsigned long __libcrunch_created_invalid_pointer;
extern unsigned long __libcrunch_fetch_bounds_called;
extern unsigned long __libcrunch_fetch_bounds_missed_cache;

/* Only defined in the real libcrunch (preload version, not stubs) */
extern int __libcrunch_really_loaded __attribute__((weak));

void __libcrunch_uncache_is_a(const void *allocptr, size_t size);
void __libcrunch_uncache_all(const void *allocptr, size_t size);

void __libcrunch_bounds_error_at(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds, const void *addr);
void __libcrunch_soft_deref_error_at(const void *ptr, __libcrunch_bounds_t bounds, const void *addr);

void mmap_replacement(struct generic_syscall *s, post_handler *post) __attribute__((visibility("hidden")));

#endif
