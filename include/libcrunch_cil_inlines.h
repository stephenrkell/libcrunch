#ifndef LIBCRUNCH_CIL_INLINES_H_
#define LIBCRUNCH_CIL_INLINES_H_

// #define LIBCRUNCH_WORDSIZE_BOUNDS 1 /* HACK temporary */

/* NO -- assume uniqtype is already defined, e.g. by -include */
// #include "uniqtype.h"

#ifndef assert
#define __libcrunch_defined_assert
//#ifdef DEBUG
#define assert(cond) \
	if (!cond) abort()
//#else
//#define assert(cond)
//#endif
#endif

/* Ideally we really want to fit in 64 bits on x86-64. 
 * This makes life a bit trickier, however. 
 * For now, we support both representations, using conditional compilation. */
struct __libcrunch_bounds_s
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* We store only the lower 32 bits of the base.
	 * We infer the upper 32 bits from those of the derivedfrom ptr,
	 * and from the inequality
	 *    base <= ptr < base + size
	 * ... see __libcrunch_get_base and __libcrunch_get_limit.
	 */
	unsigned long base:32;
	unsigned long size:32;
#else
	/* A <base, size> representation has certain advantages over <base, limit>. 
	 * In particular, our fast-path test uses the size, not the limit. */
	unsigned long base;
	unsigned long size;
#endif
};
typedef struct __libcrunch_bounds_s __libcrunch_bounds_t;

/// / HACKs to get us going
// #define LIBCRUNCH_BOUNDS_REGION_BASE ((void*) 0x300000000000ul)
// #define LIBCRUNCH_BOUNDS_REGION_SIZE          0x400000000000ul
// /* The bounds region is n/(n+1) of the address space size, 
//  * where n is the ratio of bounds size to pointer size.
//  * We round it down to 40 bits (FIXME: safe? not really) */
// // #ifdef LIBCRUNCH_WORDSIZE_BOUNDS
// // #define BOUNDS_RATIO 1
// // #else
// // #define BOUNDS_RATIO 2
// // #endif
// // #define LIBCRUNCH_BOUNDS_REGION_SIZE ( \
// //     ((((((1ul<<47) * BOUNDS_RATIO) / (BOUNDS_RATIO + 1)) >> 40) /*+ 1*/) << 40) \
// // )

void **__libcrunch_bounds_bases_region_00;
void **__libcrunch_bounds_bases_region_2a;
void **__libcrunch_bounds_bases_region_7a;
unsigned long *__libcrunch_bounds_sizes_region_00;
unsigned long *__libcrunch_bounds_sizes_region_2a;
unsigned long *__libcrunch_bounds_sizes_region_7a;

#ifndef unlikely
#define __libcrunch_defined_unlikely
#define unlikely(cond) (__builtin_expect( (cond), 0 ))
#endif
#ifndef likely
#define __libcrunch_defined_likely
#define likely(cond)   (__builtin_expect( (cond), 1 ))
#endif

/* Our functions are *not* weak -- they're defined in the noop library. 
 * (We would like the noop library not to be necessary.) */

int __libcrunch_global_init (void);

#if !defined(NO_PURE) && !defined(PURE)
#define PURE __attribute__((pure))
#elif !defined(PURE)
#define PURE
#endif

/* Type checking */
int __is_a_internal(const void *obj, const void *u) PURE;
int __like_a_internal(const void *obj, const void *u) PURE;
int __named_a_internal(const void *obj, const void *u) PURE;
int __is_a_function_refining_internal(const void *obj, const void *u) PURE;
int __is_a_pointer_of_degree_internal(const void *obj, int d) PURE;
int __can_hold_pointer_internal(const void *obj, const void *value) PURE;

/* Bounds checking */
__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *derived_ptr, struct uniqtype *u) PURE;
void __libcrunch_bounds_error(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds);
void * __check_derive_ptr_internal(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t *derivedfrom_bounds, struct uniqtype *t)/* PURE -- fixme */;

/* Utilities */
// FIXME: is it okay that this is weak? I think we don't use it anyway
const void *__libcrunch_typestr_to_uniqtype (const char *) __attribute__((weak));
/* This is not weak. */
void __assert_fail(const char *__assertion, const char *__file,
    unsigned int __line, const char *__function);
void abort(void) __attribute__((noreturn));

extern _Bool __libcrunch_is_initialized __attribute__((weak));
extern unsigned long __libcrunch_begun __attribute__((weak));
extern unsigned long __libcrunch_aborted_typestr __attribute__((weak));
extern unsigned long __libcrunch_succeeded __attribute__((weak));
extern unsigned long __libcrunch_failed __attribute__((weak));
extern unsigned long __libcrunch_is_a_hit_cache __attribute__((weak));
extern unsigned long __libcrunch_checked_pointer_adjustments __attribute__((weak));
extern unsigned long __libcrunch_created_invalid_pointer __attribute__((weak));
extern unsigned long __libcrunch_fetch_bounds_called __attribute__((weak));
extern unsigned long __libcrunch_fetch_bounds_missed_cache __attribute__((weak));

/* tentative cache entry redesign to integrate bounds and types:
 * 
 * - lower
 * - upper     (one-past)
 * - t         (may be null, i.e. bounds only)
 * - sz        (size of t)
 * - period    (need not be same as period, i.e. if T is int, alloc is array of stat, say)
 *                 ** ptr arithmetic is only valid if sz == period
 *                 ** entries with sz != period are still useful for checking types 
 * - results   (__is_a, __like_a, __locally_like_a, __is_function_refining, ... others?)
 */

struct __libcrunch_cache_entry_s
{
	const void *obj_base;
	const void *obj_limit;
	struct uniqtype *uniqtype;
	unsigned period;
	unsigned short result;
	unsigned char prev_mru;
	unsigned char next_mru;
	/* TODO: do inline uniqtype cache word check? */
} __attribute__((aligned(64)));

#ifndef LIBCRUNCH_MAX_IS_A_CACHE_SIZE
#define LIBCRUNCH_MAX_IS_A_CACHE_SIZE 8
#endif
struct __libcrunch_cache
{
	unsigned int validity; /* does *not* include the null entry */
	const unsigned short size_plus_one; /* i.e. including the null entry */
	unsigned short next_victim;
	unsigned char head_mru;
	unsigned char tail_mru;
	/* We use index 0 to mean "unused" / "null". */
	struct __libcrunch_cache_entry_s entries[1 + LIBCRUNCH_MAX_IS_A_CACHE_SIZE];
};
extern struct __libcrunch_cache /* __thread */ __libcrunch_is_a_cache;
extern struct __libcrunch_cache /* __thread */ __libcrunch_fake_bounds_cache;

#ifndef LIBCRUNCH_TRAP_TAG_SHIFT
#define LIBCRUNCH_TRAP_TAG_SHIFT 49 /* FIXME: good for x86-64, less good for others */
#endif

#define LIBCRUNCH_TRAP_ONE_PAST 1
#define LIBCRUNCH_TRAP_ONE_BEFORE 2
#define LIBCRUNCH_TRAP_INVALID 3
#define LIBCRUNCH_TRAP_TAG_WIDTH 2
#define LIBCRUNCH_TRAP_TAG_MASK (((unsigned long)((1ul<<LIBCRUNCH_TRAP_TAG_WIDTH) - 1ul)) << LIBCRUNCH_TRAP_TAG_SHIFT)


extern inline int (__attribute__((always_inline,gnu_inline)) __is_aU )(const void *obj, const void *uniqtype);

extern inline void (__attribute__((always_inline,gnu_inline)) __inline_assert)(
	int cond, const char *assertion, const char *file, unsigned int line, const char *func
		);
extern inline void (__attribute__((always_inline,gnu_inline)) __inline_assert)(
	int cond, const char *assertion, const char *file, unsigned int line, const char *func
		){
	if (!cond) __assert_fail(assertion, file, line, func);
}

extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_check_init)(void);
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_check_init)(void)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (unlikely(! & __libcrunch_is_initialized))
	{
		/* This means that we're not linked with libcrunch. 
		 * There's nothing we can do! */
		return -1;
	}
	if (unlikely(!__libcrunch_is_initialized))
	{
		/* This means we haven't initialized.
		 * Try that now (it won't try more than once). */
		int ret = __libcrunch_global_init ();
		return ret;
	}
#endif
	return 0;
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_local_bounds)(int idx, int limit);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_local_bounds)(int idx, int limit)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	/* FIXME: actually do something more sophisticated involving trap pointers here. */
	if (unlikely(idx >= limit)) abort();
#endif
}

/* FIXME: reinstate "extended counts" versions, which were

#ifdef LIBCRUNCH_EXTENDED_COUNTS
#define LIBCRUNCH_BASIC_CHECKS \
	do { \
		++__libcrunch_begun; \
		// Check for init first, else we can't use the counts. \
		\ 
		if (unlikely(__libcrunch_check_init() == -1)) \
		{ \
			++__libcrunch_begun; \
			++__libcrunch_aborted_init; \
			return 1; \
		} \
		if (!obj) \
		{ \
			++__libcrunch_begun; \
			++__libcrunch_trivially_succeeded; \
			return 1; \
		} \
	} while (0)
#else
#define LIBCRUNCH_BASIC_CHECKS \
	do { \
		if (!obj) \
		{ \
			return 1; \
		} \
		if (unlikely(__libcrunch_check_init() == -1)) \
		{ \
			return 1; \
		} \
	} while (0)
#endif

extern inline int __attribute__((always_inline,gnu_inline)) __is_aU(const void *obj, struct uniqtype *r)
{
	LIBCRUNCH_BASIC_CHECKS;
	
	// Null uniqtype means __is_aS got a bad typestring, OR we're not 
	// linked with enough uniqtypes data. 
	if (unlikely(r == NULL))
	{
		++__libcrunch_begun;
		if (__libcrunch_debug_level > 0) warnx("Aborted __is_a(%p, %p), reason: %\n", obj, r, 
			"unrecognised typename (see stack trace)");
		++__libcrunch_aborted_typestr;
		return 1;
	}
	
	if (r == (void*) &__uniqtype__signed_char || r == (void*) &__uniqtype__unsigned_char)
	{
#ifdef LIBCRUNCH_EXTENDED_COUNTS
		++__libcrunch_begun;
		++__libcrunch_trivially_succeeded;
#endif
		return 1;
	}
	
	// now we're really started
	++__libcrunch_begun;
	return __is_a_internal(obj, r);
}

extern inline int __attribute__((always_inline,gnu_inline)) __is_aS(const void *obj, const char *typestr)
{
	LIBCRUNCH_BASIC_CHECKS;
	
	const struct uniqtype * r = __libcrunch_typestr_to_uniqtype(typestr);

	return __is_aU(obj, r);
}

*/

/* To implement a get_pc() function, we'd like to use __builtin_return_address
 * which means the function really needs to be noinline -- but also static,
 * to avoid multiple-definition link errors. This causes the compiler to 
 * complain (warn) when it's used from extern inline functions, like all
 * our functions here. HMM. Instead, ditch __builtin_return_address in favour
 * of some different GNU extensions: "&&label" and gnu_inline. ACTUALLY that
 * doesn't work on my current gcc (4.9.2) because it mistakenly thinks that 
 * "&&mylabel" is a local variable, then does bogus UB optimisations on the
 * result. SIGH. Use an arch-specific sequence instead. DOUBLE SIGH: if we
 * write "callq 0" it gets relocated, so use .byte. */
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_pc)(void);
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_pc)(void)
{
	// mylabel: return &&mylabel;
	void *addr;
	__asm__ volatile (".byte 0xe8   # callq \n\
	                   .byte 0x0    # 0 \n\
	                   .byte 0x0    # \n\
	                   .byte 0x0    # \n\
	                   .byte 0x0    # \n\
	                   pop %0" : "=r"(addr)); /* FIXME: CIL frontc parse bug: */ // : /* no inputs */ : /* no clobbers */);
	return addr;
}
void warnx(const char *fmt, ...);

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val, unsigned long from_size);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val, unsigned long from_size)
{
#ifdef LIBCRUNCH_TRACE_WIDEN_INT_TO_POINTER
	/* To get a return address, use a noinline nested function. */
	if (from_size < sizeof (void*)) warnx("Unsafe integer-to-pointer cast of value %llx %at %p\n", val, __libcrunch_get_pc());
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_cache_sanity )(struct __libcrunch_cache *cache);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_cache_sanity )(struct __libcrunch_cache *cache)
{
#ifdef DEBUG
	unsigned visited_linear = 0u;
	for (int i = 1; i < cache->size_plus_one; ++i)
	{
		if (cache->validity & (1<<(i-1)))
		{
			visited_linear |= (1<<(i-1));
		}
	}
	unsigned visited_mru = 0u;
	for (unsigned char i = cache->head_mru; i != 0; i = cache->entries[i].next_mru)
	{
		assert(cache->validity & (1<<(i-1)));
		// assert we haven't been here before
		assert(!(visited_mru & (1<<(i-1))));
		visited_mru |= (1<<(i-1));
	}
	assert(visited_linear == visited_mru);
	// go the other way too
	unsigned visited_lru = 0u;
	for (unsigned char i = cache->tail_mru; i != 0; i = cache->entries[i].prev_mru)
	{
		assert(cache->validity & (1<<(i-1)));
		// assert we haven't been here before
		assert(!(visited_lru & (1<<(i-1))));
		visited_lru |= (1<<(i-1));
	}
	assert(visited_linear == visited_lru);
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_unlink )(struct __libcrunch_cache *cache, unsigned i);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_unlink )(struct __libcrunch_cache *cache, unsigned i)
{
	__libcrunch_check_cache_sanity(cache);
	// unset validity and make this the next victim
	cache->validity &= ~(1u<<(i-1));
	cache->next_victim = i;
	// unhook us from the mru list
	unsigned char our_next = cache->entries[i].next_mru;
	unsigned char our_prev = cache->entries[i].prev_mru;
	if (our_prev) cache->entries[our_prev].next_mru = our_next;
	if (our_next) cache->entries[our_next].prev_mru = our_prev;
	if (cache->head_mru == i) cache->head_mru = our_next;
	if (cache->tail_mru == i) cache->tail_mru = our_prev;
	/* We're definitely invalid. */
	cache->validity &= ~(1u<<(i-1));
	__libcrunch_check_cache_sanity(cache);
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_push_head_mru )(struct __libcrunch_cache *cache, unsigned i);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_push_head_mru )(struct __libcrunch_cache *cache, unsigned i)
{
	__libcrunch_check_cache_sanity(cache);
	/* Put us at the head of the LRU chain. */
	cache->entries[i].prev_mru = 0;
	cache->entries[i].next_mru = cache->head_mru;
	/* Link us in at the head. */
	if (cache->head_mru != 0) cache->entries[cache->head_mru].prev_mru = (unsigned char) i;
	cache->head_mru = (unsigned char) i;
	/* Set the tail, if we didn't already have one. */
	if (cache->tail_mru == 0) cache->tail_mru = i;
	/* We're definitely valid. */
	cache->validity |= (1u<<(i-1));
	/* Should be sane again now. */
	__libcrunch_check_cache_sanity(cache);
}	

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_bump_victim )(struct __libcrunch_cache *cache, unsigned i);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_bump_victim )(struct __libcrunch_cache *cache, unsigned i)
{
	__libcrunch_check_cache_sanity(cache);
	// make sure we're not the next victim
	if (unlikely(cache->next_victim == i))
	{
		if (cache->size_plus_one > 1)
		{
			cache->next_victim = 1 + ((i + 1 - 1) % (cache->size_plus_one - 1));
		}
	}
	__libcrunch_check_cache_sanity(cache);
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_bump_mru )(struct __libcrunch_cache *cache, unsigned i);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_cache_bump_mru )(struct __libcrunch_cache *cache, unsigned i)
{
	__libcrunch_check_cache_sanity(cache);
	if (cache->head_mru != i)
	{
		if (cache->validity & (1u<<(i-1))) __libcrunch_cache_unlink(cache, i);
		__libcrunch_cache_push_head_mru(cache, i);
	}
	__libcrunch_check_cache_sanity(cache);
}

extern inline struct __libcrunch_cache_entry_s *(__attribute__((always_inline,gnu_inline)) __libcrunch_cache_lookup )(struct __libcrunch_cache *cache, const void *obj, struct uniqtype *t, unsigned long require_period);
extern inline struct __libcrunch_cache_entry_s *(__attribute__((always_inline,gnu_inline)) __libcrunch_cache_lookup )(struct __libcrunch_cache *cache, const void *obj, struct uniqtype *t, unsigned long require_period)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	__libcrunch_check_cache_sanity(cache);
#ifdef LIBCRUNCH_CACHE_LINEAR
	for (unsigned char i = 1; i < cache->size_plus_one; ++i)
#else
	for (unsigned char i = cache->head_mru; i != 0; i = cache->entries[i].next_mru)
#endif
	{
		if (cache->validity & (1<<(i-1)))
		{
			struct uniqtype *cache_uniqtype = cache->entries[i].uniqtype;
			/* We test whether the difference is divisible by the period and within the bounds */
			signed long long diff = (char*) obj - (char*) cache->entries[i].obj_base;
			if (cache_uniqtype == t
					&& (char*) obj >= (char*)cache->entries[i].obj_base
					&& (char*) obj < (char*)cache->entries[i].obj_limit
					&& 
					((diff == 0)
						|| (cache->entries[i].period != 0
							&& (!require_period || cache->entries[i].period == require_period)
							&& diff % cache->entries[i].period == 0)))
			{
				// hit
				__libcrunch_cache_bump_mru(cache, i);
				return &cache->entries[i];
			}
		}
	}
#endif
	__libcrunch_check_cache_sanity(cache);
	return ((void*)0);
}

extern inline struct __libcrunch_cache_entry_s *(__attribute__((always_inline,gnu_inline)) __libcrunch_cache_lookup_notype )(struct __libcrunch_cache *cache, const void *obj, unsigned long require_period);
extern inline struct __libcrunch_cache_entry_s *(__attribute__((always_inline,gnu_inline)) __libcrunch_cache_lookup_notype )(struct __libcrunch_cache *cache, const void *obj, unsigned long require_period)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	__libcrunch_check_cache_sanity(cache);
#ifdef LIBCRUNCH_CACHE_LINEAR
	for (unsigned char i = 1; i < cache->size_plus_one; ++i)
#else
	for (unsigned char i = cache->head_mru; i != 0; i = cache->entries[i].next_mru)
#endif
	for (i = 1; i < cache->size_plus_one; ++i)
	{
		if (cache->validity & (1<<(i-1)))
		{
			/* We test whether the difference is divisible by the period and within the bounds */
			signed long long diff = (char*) obj - (char*) cache->entries[i].obj_base;
			if ((char*) obj >= (char*)cache->entries[i].obj_base
					&& (char*) obj < (char*)cache->entries[i].obj_limit
					&& 
					((diff == 0)
						|| (cache->entries[i].period != 0
							&& (!require_period || cache->entries[i].period == require_period)
							&& diff % cache->entries[i].period == 0)))
			{
				// hit
				__libcrunch_cache_bump_mru(cache, i);
				return &cache->entries[i];
			}
		}
	}
#endif
	__libcrunch_check_cache_sanity(cache);
	return ((void*)0);
}

extern inline int (__attribute__((always_inline,gnu_inline)) __is_aU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_aU )(const void *obj, const void *uniqtype)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj) 
	{ 
		return 1; 
	} 
	if (obj == (void*) -1) 
	{ 
		return 1; 
	} 
	// int inited = __libcrunch_check_init (); 
	// if (unlikely(inited == -1))
	// { 
	//	 return 1; 
	// } 
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */ 
	if (unlikely(!uniqtype))
	{ 
	   __libcrunch_begun++; 
	   __libcrunch_aborted_typestr++; 
		 return 1; 
	} 
	/* No need for the char check in the CIL version */ 
	// now we're really started 
	__libcrunch_begun++; 

	struct __libcrunch_cache_entry_s *hit = __libcrunch_cache_lookup(&__libcrunch_is_a_cache, 
		obj, (struct uniqtype*) uniqtype, 0);
	if (hit)
	{
		// hit!
		++__libcrunch_is_a_hit_cache;
		if (hit->result) 
		{
			++__libcrunch_succeeded;
			return 1;
		}
		else
		{
			// to make sure the error message and suppression handling happen,
			// we have to call the slow-path code.
			return __is_a_internal(obj, uniqtype); 
		}
		return 1; //__libcrunch_is_a_cache[i].result;
	}
	// miss: __is_a_internal will cache if it's cacheable
	int ret = __is_a_internal(obj, uniqtype); 
	
	return ret;
#else
	return 1;
#endif
}

extern inline int (__attribute__((always_inline,gnu_inline)) __is_aS) (const void *obj, const char *typestr);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_aS) (const void *obj, const char *typestr)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init ();
	// if (unlikely(inited == -1))
	// {
	//	 return 1;
	// }

	const void * r = __libcrunch_typestr_to_uniqtype(typestr);

	int ret = __is_aU(obj, r);
	return ret;
#else
	return 1;
#endif
}

extern inline int (__attribute__((always_inline,gnu_inline)) __like_aU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __like_aU )(const void *obj, const void *uniqtype)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj) 
	{ 
		return 1; 
	} 
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init (); 
	// if (unlikely(inited == -1))
	// { 
	//	 return 1; 
	// } 
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */ 
	if (unlikely(!uniqtype))
	{ 
	   __libcrunch_begun++; 
	   __libcrunch_aborted_typestr++; 
		 return 1; 
	} 
	/* No need for the char check in the CIL version */ 
	// now we're really started 
	__libcrunch_begun++; 
	int ret = __like_a_internal(obj, uniqtype); 
	return ret;
#else
	return 1;
#endif
}

extern inline int (__attribute__((always_inline,gnu_inline)) __named_aU )(const void *obj, const char *s);
extern inline int (__attribute__((always_inline,gnu_inline)) __named_aU )(const void *obj, const char *s)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}

	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */
	if (unlikely(!s))
	{
		__libcrunch_begun++;
		__libcrunch_aborted_typestr++;
		return 1;
	}
	/* No need for the char check in the CIL version */ 
	// now we're really started 
	__libcrunch_begun++;
	int ret = __named_a_internal(obj, s);
	return ret;
#else
	return 1;
#endif
}
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_function_refiningU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_function_refiningU )(const void *obj, const void *uniqtype)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init ();
	// if (unlikely(inited == -1))
	// {
	//	 return 1;
	// }
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not 
	 * linked with enough uniqtypes data. */
	if (unlikely(!uniqtype))
	{
		__libcrunch_begun++;
		__libcrunch_aborted_typestr++;
		return 1;
	}
	// now we're really started
	__libcrunch_begun++;
	int ret = __is_a_function_refining_internal(obj, uniqtype);
	return ret;
#else
	return 1;
#endif
}
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_pointer_of_degree)(const void *obj, int d);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_pointer_of_degree)(const void *obj, int d)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	if (d == 0) return 1;
	
	__libcrunch_begun++;
	int ret = __is_a_pointer_of_degree_internal(obj, d);
	return ret;
#else
	return 1;
#endif
}
extern inline int (__attribute__((always_inline,gnu_inline)) __can_hold_pointer)(const void *target, const void *value);
extern inline int (__attribute__((always_inline,gnu_inline)) __can_hold_pointer)(const void *target, const void *value)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	if (!target)
	{
		return 1;
	}
	if (target == (void*) -1)
	{
		return 1;
	}
	if (!value)
	{
		return 1;
	}
	if (value == (void*) -1)
	{
		return 1;
	}
	__libcrunch_begun++;
	int ret = __can_hold_pointer_internal(target, value);
	return ret;
#else
	return 1;
#endif
}
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_trap)(const void *ptr, unsigned short tag);
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_trap)(const void *ptr, unsigned short tag)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	/* use XOR to allow kernel-mode pointers too (even though we generally don't support these) */
	return (void *)(((unsigned long long) ptr) ^ (((unsigned long long) tag) << LIBCRUNCH_TRAP_TAG_SHIFT));
#else
	return ptr;
#endif
}
/* We use this one in pointer differencing and cast-to-integer. 
 * We return an unsigned long to avoid creating a pointless cast *back* to pointer. 
 * Instead, when doing pointer differencing, crunchbound takes on the task 
 * of the scaling the difference by the pointer target type size. */
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_detrap)(const void *any_ptr);
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_detrap)(const void *any_ptr)
{
#ifndef LIBCRUNCH_NOOP_INLINES
// 	/* Recall that traps work by XORing with the non-canonical bits of the pointer,
// 	 * which may be 0 or 1. So to de-trap, we can't just unconditionally "clear" 
// 	 * or "set" those bits; it depends on whether the pointer is positive or negative.
// 	 * First clear them, then OR in all the bits again if it's negative. */
	unsigned long val = (unsigned long) any_ptr;
// 	return 
// 			(val & ~LIBCRUNCH_TRAP_TAG_MASK)
// 			| (((signed long) any_ptr) < 0 ? LIBCRUNCH_TRAP_TAG_MASK : 0);
	// An experiment with "shift left, shift right" logic.
	// This means leaving one bit between address bits and the trap bits,
	// so that we avoid shifting out all the should-be-all-0-or-all-1 bits.
#define WORD_BITS (8 * sizeof (unsigned long))
	/* Leave the top one non-canonical bit of the pointer present. It will fill in the rest. */
#define SHIFT_AMOUNT (WORD_BITS - LIBCRUNCH_TRAP_TAG_SHIFT)
	return (unsigned long) (
		((signed long) (val << SHIFT_AMOUNT)) >> SHIFT_AMOUNT
	);
#undef WORD_BITS
#undef SHIFT_AMOUNT
#else
	return any_ptr;
#endif
}
/* ONLY use untrap if you're *sure* you have a trapped pointer! */
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_untrap)(const void *trapptr, unsigned short tag);
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_untrap)(const void *trapptr, unsigned short tag)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	///* XOR is handy like this */
	// return __libcrunch_trap(trapptr, tag);
	return (void*) __libcrunch_detrap(trapptr);
#else
	return trapptr;
#endif
}

extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_ptr_trap_bits)(const void *maybe_trap/*, unsigned short tag*/);
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_ptr_trap_bits)(const void *maybe_trap/*, unsigned short tag*/)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	/* FIXME: this is all very archdep */
	unsigned long addr = (unsigned long) maybe_trap;
	unsigned long bits = addr;
	bits >>= LIBCRUNCH_TRAP_TAG_SHIFT;
	/* If we have a kernel-mode pointer, our bits will be inverted.
	 * Signed-right-shift the addr, giving us all 0s or all 1s, then XOR.
	 * FIXME: are these two instructions really worth it? Skip kernel-mode support? */
#ifndef LIBCRUNCH_NO_KERNEL_POINTERS
	/* Shift the top bit all the way down, with sign extension; we get all 1s or all 0s.
	 * XOR that with the bits we have. */
	bits ^= (unsigned long)(((signed long) addr) >> (sizeof (long) * 8 - 1));
#endif
	/* We only want the lower bits. */
	bits &= ((1ul << LIBCRUNCH_TRAP_TAG_WIDTH) - 1ul);
	return bits;
// old is_trap_ptr code was:
//	signed long long trapi = (signed long long) maybe_trap;
//	return (trapi > 0 && trapi >= (1ull << LIBCRUNCH_TRAP_TAG_SHIFT))
//			|| (trapi < 0 && 
//				/* Two's complement: the "most bits flipped" negative numbers are *closer* to 0,
//				 * e.g. all-Fs is -1 */
//				trapi <= -(1ull << LIBCRUNCH_TRAP_TAG_SHIFT));
//	/* i.e. trap values are the really-really-positive and really-really-negative addresses. */
#else
	return 0;
#endif
}
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_is_trap_ptr)(const void *maybe_trap/*, unsigned short tag*/);
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_is_trap_ptr)(const void *maybe_trap/*, unsigned short tag*/)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	// return __libcrunch_ptr_trap_bits(maybe_trap) != 0;
	return __libcrunch_detrap(maybe_trap) != (unsigned long) maybe_trap;
#else
	return 0;
#endif
}
/*
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(const __libcrunch_bounds_t *in);
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(const __libcrunch_bounds_t *in)
{
	return in->base == (void*) -1;
}
*/
#define _CLEAR_UPPER_32(i) \
(((unsigned long) (i)) & ((1ul<<32)-1ul))
#define _CLEAR_LOWER_32(i) \
(((unsigned long) (i)) & ~((1ul<<32)-1ul))

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __make_bounds)(unsigned long base, unsigned long limit);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __make_bounds)(unsigned long base, unsigned long limit)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return (__libcrunch_bounds_t) {
			((unsigned long) _CLEAR_UPPER_32(base)),
			limit - base
	};
#else
	return (__libcrunch_bounds_t) { base, (char*) limit - (char*) base };
#endif
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_max_bounds)(const void *ptr);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_max_bounds)(const void *ptr)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* Max bounds are when the two values are equal, 
	 * because this always represents a 4GB region.
	 * WHERE should we centre the bounds?
	 * Let's play it safe and make them obj += 2GB.
	 * Note that in extreme cases, this will actually not permit
	 * some accesses which the intention of "max bounds" is to include. */
	return __make_bounds(((unsigned long) ptr) - 1ul<<31, ((unsigned long) ptr) - 1 + 1ul<<31);
#else
	return __make_bounds((unsigned long) 0, (unsigned long) -1);
#endif
}

extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_base)(__libcrunch_bounds_t bounds, const void *derivedfrom);
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_base)(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* The bounds are storing only the lower bits of the base.
	 * If they're <= derivedfrom's lower 32, they are shared with the base.
	 * Otherwise, the base is 4GB lower.
	 * (base <= derivedfrom < limit)
	 */
	if (likely(bounds.base <= _CLEAR_UPPER_32(derivedfrom)))
	{
		return (void*) (_CLEAR_LOWER_32(derivedfrom) + bounds.base);
	} else return (void*) (_CLEAR_LOWER_32(derivedfrom) - 0x100000000ul + bounds.base);
#else
	return (void*) bounds.base;
#endif
}

extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_limit)(__libcrunch_bounds_t bounds, const void *derivedfrom);
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_limit)(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return (char*) __libcrunch_get_base(bounds, derivedfrom)
		 + __libcrunch_get_size(bounds, derivedfrom);
#else
	return (char*) bounds.base + bounds.size;
#endif
}

extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_get_size)(__libcrunch_bounds_t bounds, const void *derivedfrom);
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_get_size)(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
	return bounds.size;
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_make_invalid_bounds)(const void *ptr);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_make_invalid_bounds)(const void *ptr)
{
	/* Regardless of representation, to make invalid bounds we choose the ptr 
	 * itself as the base, and 0 as the size. This *always* fails the Austin
	 * check, because addr - base == 0, i.e. NOT (addr - base < size). */
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return (__libcrunch_bounds_t) { .base =  _CLEAR_UPPER_32(ptr), .size = 0 };
#else
	/* remember! must fail the Austin test, which is 
	 *      addr - base < size
	 * e.g. 1M   - 1M   < 0      which is false.
	 */
	return (__libcrunch_bounds_t) { .base = (unsigned long) ptr, .size = 0 };
#endif
}

extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(__libcrunch_bounds_t bounds, const void *ptr);
extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(__libcrunch_bounds_t bounds, const void *ptr)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return bounds.size == 0;
#else
	return bounds.size == 0;
#endif
}

extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_valid_bounds_equal)(__libcrunch_bounds_t bounds1, __libcrunch_bounds_t bounds2);
extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_valid_bounds_equal)(__libcrunch_bounds_t bounds1, __libcrunch_bounds_t bounds2)
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return bounds1.base == bounds2.base && bounds1.size == bounds2.size;
#else
	return bounds1.base == bounds2.base && bounds1.size == bounds2.size;
#endif
}
#undef _CLEAR_LOWER_32
#undef _CLEAR_UPPER_32

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds_from_cache)(const void *ptr, const void *derived_ptr_maybetrapped, struct uniqtype *t, unsigned long t_sz);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds_from_cache)(const void *ptr, const void *derived_ptr_maybetrapped, struct uniqtype *t, unsigned long t_sz)
{
	/* We understand trap ptrs */
	const void *testptr;
	if (unlikely(__libcrunch_ptr_trap_bits(ptr) == LIBCRUNCH_TRAP_ONE_PAST))
	{
		testptr = ((const char*) __libcrunch_untrap(ptr, LIBCRUNCH_TRAP_ONE_PAST)) - /* t->pos_maxoff */ t_sz;
	} else testptr = ptr;
	const void *derived = (const void *) __libcrunch_detrap(derived_ptr_maybetrapped);
	
	/* If we hit the is-a cache, we can return an answer inline. */
	struct __libcrunch_cache_entry_s *hit = __libcrunch_cache_lookup_notype(
			&__libcrunch_is_a_cache, testptr, /* t->pos_maxoff */ t_sz);
	if (hit)
	{
		/* Is ptr actually a t? If not, we're in trouble!
		 * Maybe it's one-past and we just de-trapped it? 
		 * NO, we handle that in caller. */
		//if (unlikely(!hit->result, 0))
		//{
		//	// loop: goto loop; /* internal error! this shouldn't happen! etc. */
		//	__asm__ volatile ("ud2");
		//}
		/* HMM. Commented this abort out since I'm now less sure that it's a good idea.
		 * See the bounds-toint test case. */
		/* Does "obj" include "derivedfrom"? */
		return __make_bounds((unsigned long) hit->obj_base, (unsigned long) hit->obj_limit);
	}
	else if (unlikely((hit = __libcrunch_cache_lookup_notype(
			&__libcrunch_fake_bounds_cache, testptr, /* t->pos_maxoff */ t_sz), hit != (void*)0)))
	{
		/* We hit an entry with no alloc base. This means it's a "fake" entry that
		 * we use to suppress repeated failures for unknown allocations. Bump up the
		 * cached limit to that it includes the derived pointer. 
		 * 
		 * ALWAYS return max bounds! These will only propagate with the pointer we're
		 * checking, so don't have a global effect (whereas the cache does). */
		char *limit_from_derived = (char*) derived
			 + (hit->uniqtype ? ((struct uniqtype *)(unsigned long) hit->uniqtype)->pos_maxoff : 1);
		if (limit_from_derived > (char*) hit->obj_limit)
		{
			hit->obj_limit = limit_from_derived;
		}
		return __libcrunch_max_bounds(derived);
	}
	return __libcrunch_make_invalid_bounds(ptr);
}

extern __libcrunch_bounds_t (__attribute__((pure)) __fetch_bounds_ool)(const void *ptr, const void *derived_ptr, struct uniqtype *t);
/* In libcrunch.c. */

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds)(const void *ptr, const void *derived_ptr, struct uniqtype *t, unsigned long t_sz);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds)(const void *ptr, const void *derived_ptr, struct uniqtype *t, unsigned long t_sz)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	++__libcrunch_fetch_bounds_called; // TEMP
	__libcrunch_bounds_t from_cache = __fetch_bounds_from_cache(
			ptr, derived_ptr, t, t_sz
	);
	if (!__libcrunch_bounds_invalid(from_cache, ptr)) return from_cache;
	/* Else if libcrunch is linked in, we can delegate to it. */
	++__libcrunch_fetch_bounds_missed_cache;
	return __fetch_bounds_internal(ptr, derived_ptr, t);
#else
	return __libcrunch_make_invalid_bounds(ptr);
#endif
}
extern inline _Bool (__attribute__((pure,always_inline,gnu_inline)) __primary_check_derive_ptr)(const void *derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t derivedfrom_bounds, unsigned long t_sz);
extern inline _Bool (__attribute__((pure,always_inline,gnu_inline)) __primary_check_derive_ptr)(const void *derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t derivedfrom_bounds, unsigned long t_sz)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	/* To make things go fast, we need to keep this to the minimum. We use the Austin et al's
	 * "unsigned subtraction" hack here (p292, Fig. 3). They write it as:
	 
        if ((unsigned)(addr-base) > size - sizeof (<type>)) FlagSpatialError();
	
	 * but for us it's more complicated because of trap values and invalid bounds.
	 * We need to make sure that invalid bounds and trap pointers *always* hit
	 * the slow path, i.e. that
	 * 
	 *       addr - base_invalid  > size_invalid - t_sz
	 * and
	 *       addr_trap - base     > size        - t_sz
	 * 
	 * ... which, it turns out, is mostly quite easy. For trap pointers
	 * it's trivial: the trap addr is *much* higher than the (non-trapped) base,
	 * so for any sensible object size (less than 2^56 bytes) we hit the ">" case.
	 * (or, in the case of kernel pointers, is *lower*, so hits the underflow case).
	 * 
	 * For invalid bounds, we must ensure that either
	 * - base is higher than addr, or
	 * - addr - base > size.      (i.e. base is *far* below addr).
	 * Doing this fast for the 64-bit bounds is not easy.
	 * Ideally we want it to hold for the *unadjusted* (raw) base/limit values.
	 * And it does! because denorm bases are by definition *higher* (in 32bit-space)
	 * than the actual base.
	 */
	// too low?
	//if (unlikely(addr < base)) { goto out_fail; }
	// NOTE: support for one-prev pointers as trap values goes here
	// too high?
	//if (unlikely(addr > limit)) { goto out_fail; }

	unsigned long addr = (unsigned long) derived;
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* FIXME: use denorm base directly. HMM. Actually, is that really faster? TEST. */
#endif
	unsigned long base = (unsigned long) __libcrunch_get_base(derivedfrom_bounds, derivedfrom);
	unsigned long size = __libcrunch_get_size(derivedfrom_bounds, derivedfrom);
	
	// return (addr - base <= size - t_sz);
	return addr - base < size;
	//if (!(addr - base < size)) abort(); else return 1;
#else
	return 1;
#endif
}

extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,3))) __secondary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *p_derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz);
extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,3))) __secondary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *p_derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	// abort();     // <-- this makes things go much faster!
	/* We're a secondary check. We assume the primary check has already happened, and failed. */
	/* We are *not* pure, although our out-of-line call *is*. */
	/* Primary might have failed because our local bounds are invalid. */
	/* Primary might have failed because we need to trap the derived pointer. */
	/* Primary might have failed because the derived-from pointer is trapped. */
	/* Primary might have failed because we have fake bounds and they need widening.
	 *     HMM. Actually this doesn't happen, because *local* fake bounds are max-bounds.
	 *     We only widen fake bounds if we lack local bounds, hit the cache, then find derived is OOB. */
	unsigned long addr = (unsigned long) *p_derived;
	unsigned long base = (unsigned long) __libcrunch_get_base(*p_derivedfrom_bounds, derivedfrom);
	unsigned long size = __libcrunch_get_size(*p_derivedfrom_bounds, derivedfrom);
	if (!(addr - base >= size)) __builtin_unreachable();
	
	// new approach:
	// unconditionally de-trap addr and, if wordsize, derivedfrom;
	addr = __libcrunch_detrap((const void *) addr);
	_Bool derivedfrom_trapped = __libcrunch_is_trap_ptr(derivedfrom);
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	derivedfrom = __libcrunch_detrap(derivedfrom);
#endif
	// ensure valid bounds
	if (__libcrunch_bounds_invalid(*p_derivedfrom_bounds, derivedfrom))
	{
		/* In the out-of-line fetch-bounds path is the code for cache lookup, 
		 * fake bounds handling and calling liballocs. 
		 * PROBLEM: to maximise our use of purity, we don't want to pass 
		 * both *p_derived and derivedfrom.
		 * -- not much solution that I can see, because of fake bounds creation
		 * PROBLEM: one of these pointers might currently be trapped.
		 * -- AH, fetch_bounds deals with this.
		 */
		*p_derivedfrom_bounds = __fetch_bounds_ool(derivedfrom, *p_derived, t);
	}
	// tell the compiler that we always get back valid bounds
	if (__libcrunch_bounds_invalid(*p_derivedfrom_bounds, derivedfrom)) __builtin_unreachable();
	// recompute base and size, from the new bounds
	base = (unsigned long) __libcrunch_get_base(*p_derivedfrom_bounds, derivedfrom);
	size = __libcrunch_get_size(*p_derivedfrom_bounds, derivedfrom);
	/* ... and NOTE that this is a no-op unless either we're wordsize 
	 *                      (=> untrapped derivedfrom gives new base)
	 *                                  or if we fetched bounds (above). */
	// do the primary check again
	// -- write now-untrapped addr back, if derivedfrom was trapped
	if (addr - base < size)
	{
		if (derivedfrom_trapped) *p_derived = (const void*) addr;
		return 1;
	}
	
	if (addr - base == size)
	{
		*p_derived = __libcrunch_trap(*p_derived, LIBCRUNCH_TRAP_ONE_PAST);
		return 1;
	}
	
	/* Note that we NEVER create trapped pointers from fake bounds. 
	 * The reason is that in fake cases, the local bounds are always max_bounds. */

	/* Handle the error. */
	__libcrunch_bounds_error(*p_derived, derivedfrom, *p_derivedfrom_bounds);
	*p_derived = __libcrunch_trap(*p_derived, LIBCRUNCH_TRAP_INVALID);
	return 0;
#else
	return 1;
#endif
}
extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,2,3))) __full_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz);
extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,2,3))) __full_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	/* PRECONDITIONS (a.k.a. things we don't need to check here): 
	 * - derivedfrom is an in-bounds pointer that really does point to a T
	   ... OR is a trap value
	 * - the byte-address derived is a multiple of t->pos_maxoff away from derivedfrom
	   ... AFTER it is converted back from a trap value
	 */

	_Bool ok = __primary_check_derive_ptr(*p_derived, derivedfrom, *derivedfrom_bounds, t_sz);
	if (likely(ok))
	{
		// tell the compiler this means our bounds are definitely valid
		if (__libcrunch_bounds_invalid(*derivedfrom_bounds, derivedfrom)) __builtin_unreachable();
		// also tell it that derivedfrom is not a trap pointer
		if (__libcrunch_is_trap_ptr(derivedfrom)) __builtin_unreachable();
		return 1;
	}
	else return __secondary_check_derive_ptr(p_derived, derivedfrom, derivedfrom_bounds, t, t_sz);
#else
	return 1;
#endif
}
extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __store_pointer_nonlocal)(const void **dest, const void *val, __libcrunch_bounds_t val_bounds);
extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __store_pointer_nonlocal)(const void **dest, const void *val, __libcrunch_bounds_t val_bounds)
{
	// do the write -- do we have to do this? NO, CIL leaves the write intact.
	// *dest = val;
	unsigned long dest_addr = (unsigned long) dest;
// #define WORD_BITS (8 * sizeof (unsigned long))
// #define SHIFT_AMOUNT (WORD_BITS - 47) /* x86_64-specific! */
// 	/* The range of addresses we shadow is
// 	 * --------------
// 	 * 2fff ffff ffff
// 	 * 2e00 0000 0000
// 	 * ...
// 	 * 0100 0000 0000
// 	 * 0000 0000 0000 ______ wraparound
// 	 * 7f00 0000 0000
// 	 * ...
// 	 * 7600 0000 0000
// 	 * 7500 0000 0000
// 	 * --------------
// 	 * The "slot number" for a ptr stored at p
// 	 * is a 44-bit number. It is the top 44 bits of
// 	 * (p + 0xa000 0000 0000)     with addition modulo 2^47, i.e. 7xxx addresses wrap around
// 	 * The bounds region begins at 0x3000 0000 0000.
// 	 * The address in the bounds region is the base plus twice the slot number.
// 	 * top44( (p + 0xa000 0000 0000)_47 ) << 1 + 0x3000 0000 0000
// 	 * HMM. I think we can't get away without two additions, unless bounds are wordsize.
// 	 * */
// 	unsigned long dest_slot_number
// 	 = ((dest_addr << SHIFT_AMOUNT) + (0xa00000000000ul << SHIFT_AMOUNT)) >> 20 /* 44 bits */;
// 	unsigned long bounds_addr = ((unsigned long) LIBCRUNCH_BOUNDS_REGION_BASE)
// 	  + dest_slot_number * sizeof (__libcrunch_bounds_t);
// #undef WORD_BITS
// #undef SHIFT_AMOUNT

	/* HMM. Stick with XOR top-three-bits thing for the base.
	 * we need {0000,0555} -> {7000,7555}
	 *         {2aaa,2fff} -> {5aaa,5fff}
	 *         {7aaa,7fff} -> {0aaa,0fff}
	 * What about sizes? 
	 * First we want to divide the addr by 2
	 * giving  {0000,02aa} 
	 *         {1555,1aaa} 
	 *         {3d55,3fff}
	 * Then come up with a fixed offset that we can add these to
	 * so that we don't clash with any of the above (six) ranges
	 * while leaving a good chunk of free VAS in the 3s--4s.
	 * How about 0800?
	 * giving  {0800,0aaa} 
	 *         {1d55,22aa} 
	 *         {4555,47ff}.
	 * Okay, let's try it.
	 */
	unsigned long base_stored_addr = dest_addr ^ 0x700000000000ul;
	unsigned long size_stored_addr = (dest_addr >> 1) + 0x080000000000ul;

	*((void **)         base_stored_addr) = (void*) val_bounds.base;
	*((unsigned long *) size_stored_addr) = val_bounds.size;
	
	/* FIXME: want to tell the compiler that these writes don't alias with
	 * any locals. Hm. I think it's already allowed to assume that. */
}

#ifdef __libcrunch_defined_unlikely
#undef unlikely
#endif
#ifdef __libcrunch_defined_likely
#undef likely
#endif
#ifdef __libcrunch_defined_assert
#undef assert
#endif

#endif
