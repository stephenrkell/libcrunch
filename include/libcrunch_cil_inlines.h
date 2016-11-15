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
	unsigned size/*:32*/;
	unsigned base/*:32*/;
#else
	/* A <base, size> representation has certain advantages over <base, limit>. 
	 * In particular, our fast-path test uses the size, not the limit. */
	unsigned long base;
#ifdef LIBCRUNCH_LONG_SIZE
	unsigned long size;
#else
	unsigned size;
#endif
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

#define _CLEAR_UPPER_32(i) \
(((unsigned long) (i)) & ((1ul<<32)-1ul))
#define _CLEAR_LOWER_32(i) \
(((unsigned long) (i)) & ~((1ul<<32)-1ul))

/* Type checking */
int __is_a_internal(const void *obj, const void *u) PURE;
int __like_a_internal(const void *obj, const void *u) PURE;
int __loosely_like_a_internal(const void *obj, const void *u) PURE;
int __named_a_internal(const void *obj, const void *u) PURE;
int __is_a_function_refining_internal(const void *obj, const void *u) PURE;
int __is_a_pointer_of_degree_internal(const void *obj, int d) PURE;
int __can_hold_pointer_internal(const void *obj, const void *value) PURE;

/* Bounds checking */
__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *derived_ptr, struct uniqtype *u) PURE;
void __libcrunch_bounds_error(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds);
void __libcrunch_soft_bounds_error_at(const void *ptr, __libcrunch_bounds_t bounds, const void *addr);
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
extern unsigned long __libcrunch_primary_secondary_transitions __attribute__((weak));

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
/* This mask has all bits *below* the trap bits set, but no others.
 * NOTE: kernel-pointer support is BROKEN with this definition. */
#define LIBCRUNCH_TRAP_BOTTOM_MASK ((unsigned long)((1ul<<LIBCRUNCH_TRAP_TAG_SHIFT) - 1ul))

extern inline int (__attribute__((always_inline,gnu_inline)) __is_aU )(const void *obj, const void *uniqtype);

extern inline void (__attribute__((always_inline,gnu_inline)) __inline_assert)(
	int cond, const char *assertion, const char *file, unsigned int line, const char *func
		);
extern inline void (__attribute__((always_inline,gnu_inline)) __inline_assert)(
	int cond, const char *assertion, const char *file, unsigned int line, const char *func
		){
	if (!cond) __assert_fail(assertion, file, line, func);
}

extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_detrap)(const void *any_ptr);
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_retrap)(const void *any_ptr, unsigned short tag);

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

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val __attribute__((unused)), unsigned long from_size __attribute__((unused)));
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val __attribute__((unused)), unsigned long from_size __attribute__((unused)))
{
#ifdef LIBCRUNCH_TRACE_WIDEN_INT_TO_POINTER
	/* To get a return address, use a noinline nested function. */
	if (from_size < sizeof (void*)) warnx("Unsafe integer-to-pointer cast of value %llx %at %p", val, __libcrunch_get_pc());
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_cache_sanity )(struct __libcrunch_cache *cache __attribute__((unused)));
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_check_cache_sanity )(struct __libcrunch_cache *cache __attribute__((unused)))
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
	return (void*)0;
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
	return (void*)0;
}

extern inline struct uniqtype *(__attribute__((always_inline,gnu_inline)) __libcrunch_get_cached_object_type)(const void *addr);
extern inline struct uniqtype *(__attribute__((always_inline,gnu_inline)) __libcrunch_get_cached_object_type)(const void *addr)
{
	struct __libcrunch_cache_entry_s *found = __libcrunch_cache_lookup_notype(
		&__libcrunch_is_a_cache,
		addr, 0);
	/* This will give us "zero-offset matches", but not contained matches. 
	 * I.e. we know that "addr" is a "found->uniqtype", but we pass over
	 * cases where some cached allocation spans "addr" at a non-zero offset. */
	if (found) return found->uniqtype;
	return (void*)0;
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
			return __is_a_internal((const void *) __libcrunch_detrap(obj), uniqtype); 
		}
		return 1; //__libcrunch_is_a_cache[i].result;
	}
	// miss: __is_a_internal will cache if it's cacheable
	int ret = __is_a_internal((const void *) __libcrunch_detrap(obj), uniqtype); 
	
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
	int ret = __like_a_internal((const void *) __libcrunch_detrap(obj), uniqtype); 
	return ret;
#else
	return 1;
#endif
}

extern inline int (__attribute__((always_inline,gnu_inline)) __loosely_like_aU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __loosely_like_aU )(const void *obj, const void *uniqtype)
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
	int ret = __loosely_like_a_internal((const void *) __libcrunch_detrap(obj), uniqtype); 
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
	int ret = __named_a_internal((const void *) __libcrunch_detrap(obj), s);
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
	int ret = __is_a_function_refining_internal((const void *) __libcrunch_detrap(obj), uniqtype);
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
	int ret = __is_a_pointer_of_degree_internal((const void *) __libcrunch_detrap(obj), d);
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
	int ret = __can_hold_pointer_internal((const void *) __libcrunch_detrap(target), value);
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
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_like_trapped)(const void *maybe_trapped, const void *ptr);
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_like_trapped)(const void *maybe_trapped, const void *ptr)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	return (void *)(
//		/* Top-with-trap bits of like_trapped */
//		((((unsigned long long) maybe_trapped) >> LIBCRUNCH_TRAP_TAG_SHIFT) << LIBCRUNCH_TRAP_TAG_SHIFT)
//	|   /* Bottom bits of ptr */
//		((((unsigned long long) ptr) << (8*sizeof(long long)-LIBCRUNCH_TRAP_TAG_SHIFT)
//			>> (8*sizeof(long long) - LIBCRUNCH_TRAP_TAG_SHIFT)))
		((unsigned long long) maybe_trapped) & ~LIBCRUNCH_TRAP_BOTTOM_MASK
			| ((unsigned long long) ptr) & LIBCRUNCH_TRAP_BOTTOM_MASK
	);
#else
	return ptr;
#endif
}
/* This means "ensure the trap is exactly this, no matter what it was previously. 
 * The safe was is to detrap and retrap. */
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_retrap)(const void *any_ptr, unsigned short tag);
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_retrap)(const void *any_ptr, unsigned short tag)
{
#ifndef LIBCRUNCH_NOOP_INLINES
	unsigned long val = (unsigned long) any_ptr;
#ifdef LIBCRUNCH_USING_TRAP_PTRS
#define WORD_BITS (8 * sizeof (unsigned long))
#define SHIFT_AMOUNT (WORD_BITS - LIBCRUNCH_TRAP_TAG_SHIFT)
#ifdef LIBCRUNCH_KERNEL_POINTERS
	return __libcrunch_trap((const void*) (
	/* Leave the top one non-canonical bit of the pointer present. It will fill in the rest. */
		((signed long) (val << SHIFT_AMOUNT)) >> SHIFT_AMOUNT
	), tag);
#else
	/* Use a possibly-faster mask-based approach */
	return (((unsigned long) val) & ~LIBCRUNCH_TRAP_TAG_MASK) |
		 (((unsigned long) tag) << LIBCRUNCH_TRAP_TAG_SHIFT);
#endif
#undef WORD_BITS
#undef SHIFT_AMOUNT
#else /* no trap ptrs */
	return (unsigned long) val;
#endif
#else
	return any_ptr;
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
#ifdef LIBCRUNCH_USING_TRAP_PTRS
#define WORD_BITS (8 * sizeof (unsigned long))
	/* Leave the top one non-canonical bit of the pointer present. It will fill in the rest. */
#define SHIFT_AMOUNT (WORD_BITS - LIBCRUNCH_TRAP_TAG_SHIFT)
	return (unsigned long) (
		((signed long) (val << SHIFT_AMOUNT)) >> SHIFT_AMOUNT
	);
#undef WORD_BITS
#undef SHIFT_AMOUNT
#else /* no trap ptrs */
	return (unsigned long) val;
#endif
#else
	return any_ptr;
#endif
}
/* ONLY use untrap if you're *sure* you have a trapped pointer! */
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_untrap)(const void *trapptr, unsigned short tag __attribute__((unused)));
extern inline void *(__attribute__((always_inline,gnu_inline)) __libcrunch_untrap)(const void *trapptr, unsigned short tag __attribute__((unused)))
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
#ifdef LIBCRUNCH_USING_TRAP_PTRS
	/* FIXME: this is all very archdep */
	unsigned long addr = (unsigned long) maybe_trap;
	unsigned long bits = addr;
	bits >>= LIBCRUNCH_TRAP_TAG_SHIFT;
	/* If we have a kernel-mode pointer, our bits will be inverted.
	 * Signed-right-shift the addr, giving us all 0s or all 1s, then XOR.
	 * Are these two instructions really worth it? Skip kernel-mode support unless #defined. */
#ifdef LIBCRUNCH_KERNEL_POINTERS
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
	/* Not using trap pointers */
	return 0;
#endif
#else
	/* Noop inlines */
	return 0;
#endif
}
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_is_trap_ptr)(const void *maybe_trap/*, unsigned short tag*/);
extern inline int (__attribute__((always_inline,gnu_inline)) __libcrunch_is_trap_ptr)(const void *maybe_trap/*, unsigned short tag*/)
{
#ifndef LIBCRUNCH_NOOP_INLINES
#ifdef LIBCRUNCH_USING_TRAP_PTRS
	// return __libcrunch_ptr_trap_bits(maybe_trap) != 0;
	return __libcrunch_detrap(maybe_trap) != (unsigned long) maybe_trap;
#else
	return 0;
#endif
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

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_max_bounds)(const void *ptr __attribute__((unused)));
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __libcrunch_max_bounds)(const void *ptr __attribute__((unused)))
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* Max bounds are when the two values are equal, 
	 * because this always represents a 4GB region.
	 * WHERE should we centre the bounds?
	 * The biggest bounds we can give are 
	 * with a base at the next-lower 4GB boundary, and 
	 * a 4GB -1 size.
	 * Note that in extreme cases, this will actually not permit
	 * some accesses which the intention of "max bounds" is to include. */
	return __make_bounds(_CLEAR_LOWER_32((unsigned long) ptr),
			_CLEAR_LOWER_32((unsigned long) ptr) + ((unsigned) -1));
#else
	return __make_bounds((unsigned long) 0, (unsigned long) -1);
#endif
}

extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_base)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)));
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_base)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)))
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* The bounds are storing only the lower bits of the base.
	 * If they're <= derivedfrom's lower 32, they are shared with the base.
	 * Otherwise, the base is 4GB lower.
	 * (base <= derivedfrom < limit).
	 * 
	 * And beware: there's an interaction with trap pointers here!
	 * We don't want to take the high-order trap bits of a trap pointer
	 * and just pretend that our real pointer sits within "size" of that.
	 * It's important that we get the "real" base so that the primary
	 * check fails and we do the detrap stuff from __secondary_check.
	 */
#ifndef LIBCRUNCH_NO_DENORM_BOUNDS
	if (likely(bounds.base <= _CLEAR_UPPER_32(derivedfrom)))
	{
#endif
		const void *ptr = (const void *) __libcrunch_detrap(derivedfrom);
		return (void*) (_CLEAR_LOWER_32(ptr) + bounds.base);
#ifndef LIBCRUNCH_NO_DENORM_BOUNDS
	} else return (void*) (_CLEAR_LOWER_32(derivedfrom) - 0x100000000ul + bounds.base);
#endif
#else
	return (void*) bounds.base;
#endif
}

extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_get_size)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)));
extern inline unsigned long (__attribute__((always_inline,gnu_inline)) __libcrunch_get_size)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)))
{
	return bounds.size;
}

extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_limit)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)));
extern inline void * (__attribute__((always_inline,gnu_inline)) __libcrunch_get_limit)(__libcrunch_bounds_t bounds, const void *derivedfrom __attribute__((unused)))
{
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	return (char*) __libcrunch_get_base(bounds, derivedfrom)
		 + __libcrunch_get_size(bounds, derivedfrom);
#else
	return (char*) bounds.base + bounds.size;
#endif
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

extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(__libcrunch_bounds_t bounds, const void *ptr __attribute__((unused)));
extern inline _Bool (__attribute__((always_inline,gnu_inline)) __libcrunch_bounds_invalid)(__libcrunch_bounds_t bounds, const void *ptr __attribute__((unused)))
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

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds_from_cache)(const void *ptr, const void *derived_ptr_maybetrapped, struct uniqtype *t __attribute__((unused)), unsigned long t_sz);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __fetch_bounds_from_cache)(const void *ptr, const void *derived_ptr_maybetrapped, struct uniqtype *t __attribute__((unused)), unsigned long t_sz)
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
extern __libcrunch_bounds_t (__attribute__((pure)) __fetch_bounds_ool_via_dladdr)(const void *ptr, const void *derived_ptr, struct uniqtype *t);
/* Both in libcrunch.c. */

extern inline _Bool (__attribute__((always_inline,gnu_inline)) __primary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t derivedfrom_bounds, unsigned long t_sz __attribute__((unused)));
extern inline _Bool (__attribute__((always_inline,gnu_inline)) __primary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t derivedfrom_bounds, unsigned long t_sz __attribute__((unused)))
{
#ifndef LIBCRUNCH_NOOP_INLINES
#ifndef LIBCRUNCH_USING_TRAP_PTRS
	return 1;
#else
	/* To make things go fast, we need to keep this to the minimum. We use the Austin et al's
	 * "unsigned subtraction" hack here (p292, Fig. 3). They write it as:
	 
        if ((unsigned)(addr-base) > size - sizeof (<type>)) FlagSpatialError();
	
	 * but for us it's more complicated because of trap values, invalid bounds,
	 * and denorm bounds (if we care about those). Generally though, it's easy
	 * to make all of these fail the basic Austin check:
	 * 
	 *      addr - base < size
	 * 
	 * ... but we try to handle trap pointers here.
	 */

	unsigned long addr = (unsigned long) *p_derived;
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	/* If we have denorms, we will use the denorm base directly and fail over to secondary
	 * checks. HMM. Actually, is that really faster? FIXME: TEST. */
	unsigned long like_trapped_base = _CLEAR_LOWER_32((unsigned long) derivedfrom) | derivedfrom_bounds.base;
	unsigned long base = (unsigned long) __libcrunch_detrap(like_trapped_base);
#else
	/* Here 'base' is never trapped. */
	unsigned long base = (unsigned long) __libcrunch_get_base(derivedfrom_bounds, derivedfrom);
	unsigned long like_trapped_base = (unsigned long) __libcrunch_like_trapped(derivedfrom, (void*) base);
#endif
	unsigned long size = __libcrunch_get_size(derivedfrom_bounds, derivedfrom);
	_Bool success;
#if defined(LIBCRUNCH_NO_SECONDARY_PATH) && defined(LIBCRUNCH_USING_TRAP_PTRS)
	/* No secondary path, so have to handle traps here too -- both "one past" (trap)
	 * and "back in" (detrap) cases.
	 *
	 * The case split is interesting:
	 *    neither trapped, Austin check passes (<): normal    (can detrap -- is noop)
	 *    both trapped,  Austin check passes (<): went back in bounds, so detrap. 
	 *    neither trapped, Austin ==:    need to trap     i.e. unconditionally set the trap
	 *    both trapped,    Austin ==:    *leave* trapped  ... in both cases (assumes one-past trap)
	 *    neither trapped, Austin >:     fail  (MUST abort -- need compiler opts)
	 *    both trapped,    Austin >:     fail  (MUST abort -- need compiler opts)
	 *
	 * -- do this!
	 */
	if (likely(addr - like_trapped_base <= size))
	{
		if (unlikely(addr - base == size))
		{
			// FIXME: needs to be "re-trap", i.e. no-op if derived was already trapped
			*p_derived = __libcrunch_trap(*p_derived, LIBCRUNCH_TRAP_ONE_PAST); // TODO: try with conditional trap
		}
		else
		{
			// no-op if derived was already untrapped
			*p_derived = __libcrunch_detrap(*p_derived); // TODO: try with conditional untrap
		}
		success = 1;
	} else success = 0;
	/* We seem to be out-of-bounds. What we do now is subtle.
	 * 1. Put the bad pointer in a register and do a one-byte read from it.
	 * 2. If the register has changed, it means we took a trap, decided it was
	 *    a trap pointer that had moved back in bounds, and detrapped it. Success.
	 * 3. Else abort. It might be a trap pointer that we had moved, but *not*
	 *    back into bounds and not by zero.
	 * Can the trap handler decide these things by itself? It can get the bounds
	 * for the *new* pointer by fetching them, but how does it know that's a valid
	 * adjustment of the old pointer? It depends how much we're adjusting it by.
	 * FIXME: fill this in. */
#else
#if defined(LIBCRUNCH_TRAP_ONE_PAST_IN_PRIMARY_CHECK)
	success = addr - base <= size;
	if (unlikely(addr - base == size))
	{
		*p_derived = __libcrunch_trap(*p_derived, LIBCRUNCH_TRAP_ONE_PAST);
	}
#else
	success = addr - base < size;
#endif
#endif
#if defined(LIBCRUNCH_TRACE_PRIMARY_CHECKS) && !defined(LIBCRUNCH_NO_SECONDARY_PATH)
	if (!success) warnx("Primary check failed: addr %p, base %p, size %lu", 
		(void*) addr, (void*) base, size);
#endif
#ifdef LIBCRUNCH_NO_SECONDARY_PATH
	if (!success)
	{
		__libcrunch_bounds_error(*p_derived, derivedfrom, derivedfrom_bounds);
		abort();
	}
#endif
	return success;
	//if (!(addr - base < size)) abort(); else return 1;
#endif
#else
	return 1;
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __check_deref)(const void *ptr, __libcrunch_bounds_t ptr_bounds);
extern inline void (__attribute__((always_inline,gnu_inline)) __check_deref)(const void *ptr, __libcrunch_bounds_t ptr_bounds)
{
#ifdef LIBCRUNCH_CHECK_DEREF
	unsigned long base = (unsigned long) __libcrunch_get_base(ptr_bounds, ptr);
	unsigned long size = __libcrunch_get_size(ptr_bounds, ptr);
	if (likely((unsigned long) ptr - base < size))
	{
		/* success */
	} else
	{
		__libcrunch_soft_deref_error_at(ptr, ptr_bounds, __libcrunch_get_pc());
#ifdef LIBCRUNCH_ABORT_ON_OOB
		abort();
#endif
	}
#endif
}

extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,3))) __secondary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *p_derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz __attribute__((unused)));
extern inline _Bool (__attribute__((always_inline,gnu_inline,nonnull(1,3))) __secondary_check_derive_ptr)(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *p_derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz __attribute__((unused)))
{
#ifndef LIBCRUNCH_NOOP_INLINES
#ifdef LIBCRUNCH_NO_SECONDARY_PATH
	abort();     // <-- this makes things go much faster!
#endif
	/* We're a secondary check. We assume the primary check has already happened, and failed. */
	/* We are *not* pure, although our out-of-line call *is*. */
	/* Primary might have failed because our local bounds are invalid. */
	/* Primary might have failed because we need to trap the derived pointer. */
	/* Primary might have failed because the derived-from pointer is trapped. */
	/* Primary might have failed because we have fake bounds and they need widening.
	 *     HMM. Actually this doesn't happen, because *local* fake bounds are max-bounds.
	 *     We only widen fake bounds if we lack local bounds, hit the cache, then find derived is OOB. */
	// new approach:
	// unconditionally de-trap addr and, if wordsize, derivedfrom (we need it to get the base);
	unsigned long pre_detrap_addr = (unsigned long) *p_derived;
	unsigned long addr = __libcrunch_detrap(*p_derived);
	_Bool derivedfrom_trapped = __libcrunch_is_trap_ptr(derivedfrom);
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	derivedfrom = (const void*) __libcrunch_detrap(derivedfrom);
	unsigned long naive_base = (unsigned long) __libcrunch_get_base(*p_derivedfrom_bounds, *p_derived);
#else
#define naive_base base
#endif
	unsigned long base = (unsigned long) __libcrunch_get_base(*p_derivedfrom_bounds, derivedfrom);
	unsigned long size = __libcrunch_get_size(*p_derivedfrom_bounds, derivedfrom);
	if (!(pre_detrap_addr - naive_base >= size)) __builtin_unreachable();
#ifndef LIBCRUNCH_NO_SECONDARY_PATH
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
#endif
	// recompute base and size, from the new bounds
	base = (unsigned long) __libcrunch_get_base(*p_derivedfrom_bounds, derivedfrom);
	size = __libcrunch_get_size(*p_derivedfrom_bounds, derivedfrom);
	/* ... and NOTE that this is a no-op unless either we're wordsize 
	 *                      (=> untrapped derivedfrom gives new base)
	 *                                  or if we fetched bounds (above). */
	// do the primary check again
	// -- write now-untrapped addr back, if derivedfrom was trapped
	// warnx("Got to 1, deriving %p", *p_derived);
	if (addr - base < size)
	{
		// warnx("Got to 2, deriving %p", *p_derived);
		if (derivedfrom_trapped)
		{
#ifndef LIBCRUNCH_NO_WARN_BACK_IN
			warnx("Went back in bounds at %p: %p (base %p, size %lu)", 
				__libcrunch_get_pc(), (void*) addr, (void*) base, (unsigned long) size);
#endif
			*p_derived = (const void*) addr;
		}
		else
		{
			// warnx("Got to 3, deriving %p", *p_derived);
			/* Q. When is this true?
			 * A. 
			 * NOT when deriving a one-past trapped pointer from an untrapped one
			 * (we're only handling the fully-in-bounds case here).
			 * NOT just when we're doing full checks because of an earlier failed check,
			 * but the arithmetic being checked right now is plain-old in-bounds stuff.
			 * We still *did* a primary check. */
		}
		return 1;
	}
	
	// warnx("Got to 4, deriving %p", *p_derived);
	if (unlikely(addr - base == size))
	{
#ifndef LIBCRUNCH_NO_WARN_ONE_PAST
		warnx("Created one-past pointer at %p: %p (base %p, size %lu)", 
			__libcrunch_get_pc(), (void*) addr, (void*) base, (unsigned long) size);
#endif
		*p_derived = __libcrunch_trap(*p_derived, LIBCRUNCH_TRAP_ONE_PAST);
		return 1;
	}
	
	/* Note that we NEVER create trapped pointers from fake bounds. 
	 * The reason is that in fake cases, the local bounds are always max_bounds. */

	// warnx("Got to 5, deriving %p", *p_derived);
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
#ifndef LIBCRUNCH_USING_TRAP_PTRS
	return 1;
#else
	/* PRECONDITIONS (a.k.a. things we don't need to check here): 
	 * - derivedfrom is an in-bounds pointer that really does point to a T
	   ... OR is a trap value
	 * - the byte-address derived is a multiple of t->pos_maxoff away from derivedfrom
	   ... AFTER it is converted back from a trap value
	 */

	_Bool ok = __primary_check_derive_ptr(p_derived, derivedfrom, *derivedfrom_bounds, t_sz);
	if (likely(ok))
	{
		// tell the compiler this means our bounds are definitely valid
		if (__libcrunch_bounds_invalid(*derivedfrom_bounds, derivedfrom)) __builtin_unreachable();
		// also tell it that derivedfrom is not a trap pointer
		if (__libcrunch_is_trap_ptr(derivedfrom)) __builtin_unreachable();
		return 1;
	}
	else return __secondary_check_derive_ptr(p_derived, derivedfrom, derivedfrom_bounds, t, t_sz);
#endif
#else
	return 1;
#endif
}

/* Shadow-space and bounds stack are already optional. To save noise, make sure 
 * noop-inlines also turns them off. */
#ifdef LIBCRUNCH_NOOP_INLINES

#ifndef LIBCRUNCH_NO_SHADOW_SPACE
#define LIBCRUNCH_NO_SHADOW_SPACE
#endif

#ifndef LIBCRUNCH_NO_BOUNDS_STACK
#define LIBCRUNCH_NO_BOUNDS_STACK
#endif

#endif

#define BASE_STORED(ptr) ((void**)(((unsigned long) (ptr)) ^ 0x700000000000ul))
#define SIZE_STORED(ptr) ((unsigned *)((((unsigned long) (ptr)) >> 1) + 0x080000000000ul))

extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __shadow_store_bounds_for)(void **stored_pointer_addr, __libcrunch_bounds_t val_bounds, struct uniqtype *t);
extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __shadow_store_bounds_for)(void **stored_pointer_addr, __libcrunch_bounds_t val_bounds, struct uniqtype *t)
{
	/* This is necessary for polymorphic code requiring "grubbed" pointer type info
	 * -- see crunchbound. */
	if (!t) val_bounds = __libcrunch_make_invalid_bounds(/* HACK */ *stored_pointer_addr);
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
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	unsigned long b = val_bounds.base;/* promote to full width */
	*(BASE_STORED(stored_pointer_addr)) = (void*)    b;/* (necessary hack to avoid compiler warnings) */
#else
	*(BASE_STORED(stored_pointer_addr)) = (void*)    val_bounds.base;
#endif
	*(SIZE_STORED(stored_pointer_addr)) = (unsigned) val_bounds.size;
}

extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __store_pointer_nonlocal)(const void **dest, const void *val, __libcrunch_bounds_t val_bounds, struct uniqtype *val_pointee_type);
extern inline void (__attribute__((always_inline,gnu_inline,nonnull(1))) __store_pointer_nonlocal)(const void **dest, const void *val, __libcrunch_bounds_t val_bounds, struct uniqtype *val_pointee_type)
{
#ifndef LIBCRUNCH_NO_SHADOW_SPACE
	unsigned long dest_addr __attribute__((unused)) = (unsigned long) dest;
	unsigned long base_stored_addr __attribute__((unused)) = (unsigned long) BASE_STORED(dest);
	unsigned long size_stored_addr = (unsigned long) SIZE_STORED(dest);
	
	/* Being as lazy as we can here, but no lazier:
	 * - we do the expensive ool fetch to avoid leaving stale bounds in the shadow space.
	 * - we *only* need to do this if we're writing to a location with valid bounds!
	 * PROBLEM: in some workloads, this will be pessimal because we load the pointer
	 * many times, and fetch bounds each time. AHA, but when we load the pointer and
	 * fetch bounds, we should store the bounds back where we got it from, no?
	 * Actually that's hard, because of race conditions etc..
	 * We might want to instrument *pointer loads*,
	 * and then do a reaching-definitions analysis s.t. any load that
	 * reaches an indexing or arithmetic operation
	 * is eagerly "load and ensure bounds". Can be fooled by
	 * putting a procedure boundary between the load and the indexing?
	 * Hmm, not if we make that "indexing or write or pass or return".
	 * So the only cases we don't load for are pointers that are loaded,
	 * deref'd locally and then discarded.
	 * FIXME: IMPLEMENT THIS in crunchbound. ACTUALLY: don't. Lazy is slower!
	 *
	 * The above is a good indication of how counterintuitive the performance of
	 * this stuff can be.
	 */
	_Bool existing_shadow_bounds_valid = *((unsigned *) size_stored_addr) != 0; /* bounds valid? */
	if (unlikely(
				__libcrunch_bounds_invalid(val_bounds, val)
			// && existing_shadow_bounds_valid
			))
	{
#ifndef LIBCRUNCH_NO_POINTER_TYPE_INFO
		val_bounds = val ? __fetch_bounds_ool(val, val, val_pointee_type) : __make_bounds(0, 1);
		if (__libcrunch_bounds_invalid(val_bounds, val)) __builtin_unreachable();
#else
		/* Do nothing -- can't do any better in SoftBound mode */
#endif
	}

	/* If the shadow space holds bounds, we might be invaliding them, so we must write them.
	 * Otherwise, only store bounds if we have valid bounds to begin with.
	 * HMM: what about the nocrunch case? There we ensured that
	 * the shadow space is always valid and max-bounds. The price
	 * we pay is that we always write bounds here when storing a pointer.
	 * If we didn't have valid bounds already, in val_bounds,
	 * we just loaded them! In fairness, __fetch_bounds_ool is cheap
	 * in the nocrunch case because it just returns max-bounds.
	 * So now we just take the hit of writing them to the shadow space.
	 * HMM: if we were feeling sneaky, __fetch_bounds_ool could return
	 * something invalid. That would be what we wanted here (val_bounds
	 * is dead after this call) but would cause other problems on other
	 * calls to __fetch_bounds_ool (if there are any? perhaps only in
	 * secondary checks, in which case fine?). */
	if (!__libcrunch_bounds_invalid(val_bounds, val) || existing_shadow_bounds_valid)
	{
		__shadow_store_bounds_for((void**) dest, val_bounds, val_pointee_type);
	}
	
	/* FIXME: want to tell the compiler that these writes don't alias with
	 * any locals. Hm. I think it's already allowed to assume that. */
#endif
}

/* HACK */
// extern __liballocs_walk_subobjects_spanning_rec(
// 	signed accum_offset, unsigned accum_depth,
// 	const signed target_offset_within_u,
// 	struct uniqtype *u, 
// 	int (*cb)(struct uniqtype *spans, signed span_start_offset, unsigned depth,
// 		struct uniqtype *containing, struct contained *contained_pos, 
// 		signed containing_span_start_offset, void *arg),
// 	void *arg
// 	);

/* This is now and out-of-line path. */
extern void (__attribute__((nonnull(1))) __store_pointer_nonlocal_via_voidptrptr)(const void **dest, const void *srcval, __libcrunch_bounds_t val_bounds, struct uniqtype *static_guessed_srcval_pointee_type);

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_from_shadow_space)(const void *ptr, void **loaded_from);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_from_shadow_space)(const void *ptr, void **loaded_from)
{
#ifndef LIBCRUNCH_NO_SHADOW_SPACE
	if (!ptr) return __make_bounds(0, 1);
	if (loaded_from)
	{
		unsigned long loaded_from_addr __attribute__((unused)) = (unsigned long) loaded_from;
		unsigned long base_stored_addr = (unsigned long) BASE_STORED(loaded_from);
		unsigned long size_stored_addr = (unsigned long) SIZE_STORED(loaded_from);

		__libcrunch_bounds_t b = (__libcrunch_bounds_t) {
			.base = (unsigned long) *((void **)         base_stored_addr),
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
			.size = *((unsigned *) size_stored_addr)
#else
			/* To support no-load mode, compute the size sneakily: to ensure
			 * loading "0x ffff ffff" maps to "max bounds", 
			 * we need a 32-to-64 transformation that generates
			 * "0x ffff ffff ffff ffff" from "0x ffff ffff"
			 * but leaves everything else unchanged. Below is the answer.
			 */
#ifdef LIBCRUNCH_LONG_SIZE
			.size = (
				(unsigned long) (
					*((unsigned *) size_stored_addr)
					+ 1u
				)
				- 1ul
			)
#else
			.size = *((unsigned *) size_stored_addr)
#endif
#endif
		};
#ifndef LIBCRUNCH_NO_DEBUG_SHADOW_SPACE
		if (unlikely(__libcrunch_bounds_invalid(b, ptr)))
		{
			warnx("Fetched invalid bounds for %p (loaded from %p)", ptr, loaded_from);
		}
#endif
		return b;
	}
#endif
	return __libcrunch_make_invalid_bounds(ptr);
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_inl)(const void *ptr, void **loaded_from, struct uniqtype *t);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_inl)(const void *ptr, void **loaded_from, struct uniqtype *t)
{
	/* We could choose to inline or not:
	 * - shadow-space lookup
	 * - cache lookup
	 * - whole-hog liballocs lookup.
	 *
	 * We choose just the shadow space, for now.
	 * 
	 * NOTE that __fetch_bounds_ool is exactly the complement of this function, 
	 * i.e. it does the lookups necessary assuming we, __fetch_bounds_inl, have failed. */
	return __fetch_bounds_from_shadow_space(ptr, loaded_from);
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_full)(const void *ptr, const void *derived, void **loaded_from, struct uniqtype *t);
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline,nonnull(1))) __fetch_bounds_full)(const void *ptr, const void *derived, void **loaded_from, struct uniqtype *t)
{
	if (!ptr) return __libcrunch_make_invalid_bounds(derived);
	if (!t) return __libcrunch_make_invalid_bounds(derived);
	__libcrunch_bounds_t bounds = __fetch_bounds_inl(ptr, loaded_from, t);
#ifndef LIBCRUNCH_NO_POINTER_TYPE_INFO
	if (unlikely(__libcrunch_bounds_invalid(bounds, ptr)))
	{
		bounds = __fetch_bounds_ool(ptr, derived, t);
		if (__libcrunch_bounds_invalid(bounds, ptr)) __builtin_unreachable();
	}
#endif
	return bounds;
}

extern __thread unsigned long *volatile __bounds_sp;
extern inline void *(__attribute__((always_inline,gnu_inline,malloc)) __alloc_bounds_stack_space)(unsigned long n);
extern inline void *(__attribute__((always_inline,gnu_inline,malloc)) __alloc_bounds_stack_space)(unsigned long n)
{
	__bounds_sp -= (n % sizeof (unsigned long) == 0) ? 
		(n / sizeof (unsigned long))
		: ((n / sizeof (unsigned long)) + 1);
	return __bounds_sp;
}

/* The bounds stack works as follows. 
 * 
 * - it is allocated *early*, i.e. in the shadow space initialiser, and 
 *   grows downwards like a normal stack.
 * 
 * - to call, we push right-to-left and finally push the *callee* address;
 *   this means creating a label immediately after the call
 * 
 * - to detect whether the caller pushed any bounds, we look for our
 *   current function address as the *first* thing to pop off the stack.
 *   We then immediately "consume" it, by tweaking a high bit.
 *   This prevents indirectly-recursive calls back to ourselves, 
 *   from *uninstrumented* callees, from from picking up these bounds
 *   thinking that their caller passed them. I.e. 
 * 
 *   I() calls...
 *        C () {
                   U(); // calls C         C will see bounds stack with callee C, frame address of the outer C's frame, return site in U
                   C();
          }
 *   ... need to avoid the call to C() within U() picking up bounds args passed by I.
 * 
 * - if we get it, we proceed with the pop, otherwise we use invalid bounds.
 * 
 * - on return, we don't push the return bounds unless we know the caller
 *   is going to pop them
 * 
 * - return by pushing *more* stuff on the stack; the caller cleans up the whole lot.
 * 
 * - signalling addresses: use only label addresses (call site, return site, ...?)
 * 
 * - what if the callee is uninstrumented? means top-of-stack will equal
 *   the label address... the cleanup function deals with this
 * 
 * - what about inlined callees? need to make sure that bounds-passing gets inlined too
 * 
 * - should cleanup and pop be the same function? not really because non-pointer-returning
 *   calls still need cleanup... still, could perhaps roll them together
 * 
 * - varargs also needs cleanup
 * 
 * - Q. what about a callee returning to an uninstrumented caller?
 * 
 * - A. answer: it doesn't! it knows if the caller passed bounds, 
 *          though this DOES mean that an instrumented caller *always* passes its return address
 *          even if it's not passing any bounds.
 * 
 * - Q. is there a risk of preventing inlining, or optimisation across inlining?
 * 
 * - A. obviously yes. so what do we do? 
 */

extern inline void (__attribute__((always_inline,gnu_inline)) __push_local_argument_bounds)(__libcrunch_bounds_t bounds);
extern inline void (__attribute__((always_inline,gnu_inline)) __push_local_argument_bounds)(__libcrunch_bounds_t bounds)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
	*b = bounds; /* i.e. base goes in low word, size goes in higher word. */
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	warnx("Pushed bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, (unsigned long) b->size);
#else
	warnx("Pushed bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long) b->size);
#endif
#endif
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __push_argument_bounds_base_limit)(const void *ptr __attribute__((unused)), unsigned long base, unsigned long limit);
extern inline void (__attribute__((always_inline,gnu_inline)) __push_argument_bounds_base_limit)(const void *ptr __attribute__((unused)), unsigned long base, unsigned long limit)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
	*b = __make_bounds(base, limit);
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	warnx("Pushed bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, b->size);
#else
	warnx("Pushed bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long) b->size);
#endif
#endif
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __fetch_and_push_argument_bounds)(const void *ptr, void **loaded_from, struct uniqtype *t);
extern inline void (__attribute__((always_inline,gnu_inline)) __fetch_and_push_argument_bounds)(const void *ptr, void **loaded_from, struct uniqtype *t)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
	*b = __fetch_bounds_inl(ptr, loaded_from, t);
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
	warnx("Pushed bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, (unsigned long) b->size);
#else
	warnx("Pushed bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long) b->size);
#endif
#endif
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __push_argument_bounds_cookie)(const void *callee);
extern inline void (__attribute__((always_inline,gnu_inline)) __push_argument_bounds_cookie)(const void *callee)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	void **c = __alloc_bounds_stack_space(sizeof (void*));
	*c = (void*) callee;
#else
	
#endif
}

extern inline _Bool (__attribute__((always_inline,gnu_inline)) __tweak_argument_bounds_cookie)(const void *callee);
extern inline _Bool (__attribute__((always_inline,gnu_inline)) __tweak_argument_bounds_cookie)(const void *callee)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	/* Consume the cookie, by changing it to an address which is unique to the callee 
	 * and is not the valid address of *any* callee. */
	if ((void*) *__bounds_sp == callee) { *__bounds_sp |= 0x800000000000ul; return 1; }
	else return 0;
#else
	return 0;
#endif
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __peek_argument_bounds)(_Bool really, unsigned long offset, const void *ptr, const char *debugstr __attribute__((unused)));
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __peek_argument_bounds)(_Bool really, unsigned long offset, const void *ptr, const char *debugstr __attribute__((unused)))
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	/* Were we passed anything? 
	 * If the first stack location is not equal to the call (return) site, 
	 * we weren't passed anything. How do we test? The function we're being
	 * called from might be inlined. We're definitely inlined. Hmm. 
	 * Well, if we can get the "actual frame start ip" we'd be okay. */
	if (really)
	{
		/* Bounds are either one or two words, starting at __bounds_sp + 1,
		 * but we have to account for the offset. */
		__libcrunch_bounds_t b = *(__libcrunch_bounds_t *)(__bounds_sp + 1 
			+ (offset * (sizeof (__libcrunch_bounds_t) / sizeof (*__bounds_sp))));
#ifndef LIBCRUNCH_NO_WARN_INVALID_BOUNDS
		if (unlikely(__libcrunch_bounds_invalid(b, ptr)))
		{
			warnx("Code at %p received invalid bounds at offset %d for ptr value %p (expr %s)",
				__libcrunch_get_pc(), offset, ptr, debugstr);
		}
#endif
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
		warnx("Peeked argument bounds (bsp=%p) at offset %lu: base (lower bits) %lx, size %lu", __bounds_sp, offset, (unsigned long) b.base, (unsigned long) b.size);
#else
		warnx("Peeked argument bounds (bsp=%p) at offset %lu: base %p, size %lu", __bounds_sp, offset, (void*) b.base, (unsigned long) b.size);
#endif
#endif
		return b;
	}
	else
	{
#ifndef LIBCRUNCH_NO_WARN_INVALID_BOUNDS
		warnx("Code at %p received no bounds for ptr value %p (expr %s) (uninstrumented caller)",
			__libcrunch_get_pc(), ptr, debugstr);
#endif
		return __libcrunch_make_invalid_bounds(ptr);
	}
#else
	return __libcrunch_make_invalid_bounds(ptr);
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __push_local_result_bounds)(_Bool really, __libcrunch_bounds_t bounds);
extern inline void (__attribute__((always_inline,gnu_inline)) __push_local_result_bounds)(_Bool really, __libcrunch_bounds_t bounds)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	if (really)
	{
		__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
		*b = bounds;
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
		warnx("Pushed result bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, (unsigned long) b->size);
#else
		warnx("Pushed result bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long) b->size);
#endif
#endif
	}
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __push_result_bounds_base_limit)(_Bool really, const void *ptr __attribute__((unused)), unsigned long base, unsigned long limit);
extern inline void (__attribute__((always_inline,gnu_inline)) __push_result_bounds_base_limit)(_Bool really, const void *ptr __attribute__((unused)), unsigned long base, unsigned long limit)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	if (really)
	{
		__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
		*b = __make_bounds(base, limit);
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
		warnx("Pushed result bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, (unsigned long) b->size);
#else
		warnx("Pushed result bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long *) b->size);
#endif
#endif
	}
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __fetch_and_push_result_bounds)(_Bool really, const void *ptr, void **loaded_from, struct uniqtype *t);
extern inline void (__attribute__((always_inline,gnu_inline)) __fetch_and_push_result_bounds)(_Bool really, const void *ptr, void **loaded_from, struct uniqtype *t)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	if (really)
	{
		__libcrunch_bounds_t *b = __alloc_bounds_stack_space(sizeof (__libcrunch_bounds_t));
		*b = __fetch_bounds_inl(ptr, loaded_from, t);
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
		warnx("Pushed result bounds (bsp=%p): base (lower bits) %lx, size %lu", __bounds_sp, (unsigned long) b->base, (unsigned long) b->size);
#else
		warnx("Pushed result bounds (bsp=%p): base %p, size %lu", __bounds_sp, (void*) b->base, (unsigned long) b->size);
#endif
#endif
	}
#else
	
#endif
}

extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __peek_result_bounds)(_Bool really, unsigned long offset, const void *ptr, const char *calleestr __attribute__((unused)));
extern inline __libcrunch_bounds_t (__attribute__((always_inline,gnu_inline)) __peek_result_bounds)(_Bool really, unsigned long offset, const void *ptr, const char *calleestr __attribute__((unused)))
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	/* If the cookie hasn't been tweaked, do nothing. */
	if (really)
	{
		__libcrunch_bounds_t b = *(__libcrunch_bounds_t *)(__bounds_sp 
			+ (offset * (sizeof (__libcrunch_bounds_t) / sizeof (*__bounds_sp))));
#ifndef LIBCRUNCH_NO_WARN_INVALID_BOUNDS
		if (unlikely(__libcrunch_bounds_invalid(b, ptr)))
		{
			warnx("Code at %p was returned invalid bounds (by %s) at offset %d for ptr value %p",
				__libcrunch_get_pc(), calleestr, offset, ptr);
		}
#endif
#ifdef LIBCRUNCH_TRACE_BOUNDS_STACK
#ifdef LIBCRUNCH_WORDSIZE_BOUNDS
		warnx("Peeked result bounds (bsp=%p) at offset %lu: base (lower bits) %lx, size %lu", __bounds_sp, offset, (unsigned long) b.base, (unsigned long) b.size);
#else
		warnx("Peeked result bounds (bsp=%p) at offset %lu: base %p, size %lu", __bounds_sp, offset, (void*) b.base, (unsigned long) b.size);
#endif
#endif
		return b;
	}
	else
	{
#ifndef LIBCRUNCH_NO_WARN_INVALID_BOUNDS_RETURN
		warnx("Code at %p was returned no bounds for ptr value %p (uninstrumented callee %s)",
			__libcrunch_get_pc(), ptr, calleestr);
#endif
		return __libcrunch_make_invalid_bounds(ptr);
	}
#else
	return __libcrunch_make_invalid_bounds(ptr);
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __cleanup_bounds_stack)(void *saved_ptr);
extern inline void (__attribute__((always_inline,gnu_inline)) __cleanup_bounds_stack)(void *saved_ptr)
{
#ifndef LIBCRUNCH_NO_BOUNDS_STACK
	__bounds_sp = (unsigned long *) saved_ptr;
#else
	
#endif
}

extern inline void (__attribute__((always_inline,gnu_inline)) __primary_secondary_transition)(void);
extern inline void (__attribute__((always_inline,gnu_inline)) __primary_secondary_transition)(void)
{
#ifdef LIBCRUNCH_NO_SECONDARY_PATH
	abort();
#else
	++__libcrunch_primary_secondary_transitions;
#endif
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

#undef _CLEAR_LOWER_32
#undef _CLEAR_UPPER_32

#endif
