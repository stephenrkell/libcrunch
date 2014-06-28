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

#undef debug_printf /* from liballocs */
extern const char *exe_basename __attribute__((visibility("hidden")));
extern FILE *stream_err __attribute__((visibility("hidden")));
#define debug_printf(lvl, fmt, ...) do { \
    if ((lvl) <= __libcrunch_debug_level) { \
      fprintf(stream_err, "%s: " fmt, exe_basename, ## __VA_ARGS__ );  \
    } \
  } while (0)

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
#endif
extern unsigned long __libcrunch_aborted_typestr;
extern unsigned long __libcrunch_lazy_heap_type_assignment;
extern unsigned long __libcrunch_failed;
extern unsigned long __libcrunch_failed_in_alloc;
extern unsigned long __libcrunch_succeeded;

#endif
