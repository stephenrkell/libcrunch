#ifndef LIBCRUNCH_H_
#define LIBCRUNCH_H_

/* x86_64 only, for now */
#if !defined(__x86_64__) && !defined(X86_64)
#error Unsupported architecture.
#endif

#include "memtable.h"
#include "heap_index.h"
#include "allocsmt.h"
#include <stdint.h>
#include "addrmap.h"

static inline void __attribute__((gnu_inline)) __libcrunch_ensure_init(void);

#include "libcrunch.h"

/* Copied from dumptypes.cpp */
struct rec
{
	const char *name;
	short pos_maxoff; // 16 bits
	short neg_maxoff; // 16 bits
	unsigned nmemb:12;         // 12 bits -- number of `contained's (always 1 if array)
	unsigned is_array:1;       // 1 bit
	unsigned array_len:19;     // 19 bits; 0 means undetermined length
	struct { 
		signed offset;
		struct rec *ptr;
	} contained[];
};

inline struct rec *allocsite_to_uniqtype(const void *allocsite)
{
	assert(__libcrunch_allocsmt != NULL);
	struct allocsite_entry **bucketpos = ALLOCSMT_FUN(ADDR, allocsite);
	struct allocsite_entry *bucket = *bucketpos;
	for (struct allocsite_entry *p = bucket; p; p = p->next)
	{
		if (p->allocsite == allocsite) return p->uniqtype;
	}
}

#define maximum_vaddr_range_size (4*1024) // HACK
inline struct rec *vaddr_to_uniqtype(const void *vaddr)
{
	assert(__libcrunch_allocsmt != NULL);
	struct allocsite_entry **initial_bucketpos = ALLOCSMT_FUN(ADDR, (void*)((intptr_t)vaddr | STACK_BEGIN));
	struct allocsite_entry **bucketpos = initial_bucketpos;
	do 
	{
		struct allocsite_entry *bucket = *bucketpos;
		for (struct allocsite_entry *p = bucket; p; p = p->next)
		{
			if (p->allocsite <= vaddr) return p->uniqtype;
		}
		// no match? then try the next lower bucket
		--bucketpos;
	} while ((initial_bucketpos - bucketpos) * allocsmt_entry_coverage < maximum_vaddr_range_size);
	return NULL;
}
#undef maximum_vaddr_range_size

#define maximum_static_obj_size (64*1024) // HACK
inline struct rec *static_addr_to_uniqtype(const void *static_addr, void **out_object_start)
{
	assert(__libcrunch_allocsmt != NULL);
	struct allocsite_entry **initial_bucketpos = ALLOCSMT_FUN(ADDR, (void*)((intptr_t)static_addr | (STACK_BEGIN<<1)));
	struct allocsite_entry **bucketpos = initial_bucketpos;
	do 
	{
		struct allocsite_entry *bucket = *bucketpos;
		for (struct allocsite_entry *p = bucket; p; p = p->next)
		{
			if (p->allocsite <= static_addr) 
			{
				if (out_object_start) *out_object_start = p->allocsite;
				return p->uniqtype;
			}
		}
		// no match? then try the next lower bucket
		--bucketpos;
	} while ((initial_bucketpos - bucketpos) * allocsmt_entry_coverage < maximum_static_obj_size);
	return NULL;
}
#undef maximum_vaddr_range_size

/* avoid dependency on libc headers (in this header only) */
void __assert_fail(const char *assertion, 
	const char *file, unsigned int line, const char *function);
void warnx(const char *fmt, ...);
unsigned long malloc_usable_size (void *ptr);
int strcmp(const char *, const char *);

/* our own private assert */
static inline void __libcrunch_private_assert(_Bool cond, const char *reason, 
	const char *f, unsigned l, const char *fn)
{
	if (!cond) __assert_fail(reason, f, l, fn);
}

static inline void  __attribute__((gnu_inline)) __libcrunch_ensure_init(void)
{
	__libcrunch_private_assert(__libcrunch_check_init() == 0, "libcrunch init", 
		__FILE__, __LINE__, __func__);
}

/* counters */
extern unsigned long __libcrunch_begun;
#ifdef LIBCRUNCH_EXTENDED_COUNTS
extern unsigned long __libcrunch_aborted_init;
extern unsigned long __libcrunch_trivially_succeeded_null;
#endif
extern unsigned long __libcrunch_aborted_stack;
extern unsigned long __libcrunch_aborted_static;
extern unsigned long __libcrunch_aborted_typestr;
extern unsigned long __libcrunch_aborted_unknown_storage;
extern unsigned long __libcrunch_hit_heap_case;
extern unsigned long __libcrunch_hit_stack_case;
extern unsigned long __libcrunch_hit_static_case;
extern unsigned long __libcrunch_aborted_unindexed_heap;
extern unsigned long __libcrunch_aborted_unrecognised_allocsite;
extern unsigned long __libcrunch_failed;
extern unsigned long __libcrunch_succeeded;

#endif