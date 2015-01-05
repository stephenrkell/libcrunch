#ifndef LIBCRUNCH_CIL_INLINES_H_
#define LIBCRUNCH_CIL_INLINES_H_

/* the functions are *not* weak -- they're defined in the noop library. 
 * we would like the noop library not to be necessary. */
int __libcrunch_global_init (void);
int __is_a_internal(const void *obj, const void *u);
int __like_a_internal(const void *obj, const void *u);
int __named_a_internal(const void *obj, const void *u);
int __is_a_function_refining_internal(const void *obj, const void *u);
int __is_a_pointer_of_degree_internal(const void *obj, int d);
int __can_hold_pointer_internal(const void *obj, const void *value);
/* This function is weak, but FIXME: that might be broken. */
const void *__libcrunch_typestr_to_uniqtype (const char *) __attribute__((weak));
/* This is not weak. */
void __assert_fail(const char *__assertion, const char *__file,
    unsigned int __line, const char *__function);

extern _Bool __libcrunch_is_initialized __attribute__((weak));
extern unsigned long __libcrunch_begun __attribute__((weak));
extern unsigned long __libcrunch_aborted_typestr __attribute__((weak));
extern unsigned long __libcrunch_succeeded __attribute__((weak));
extern unsigned long __libcrunch_failed __attribute__((weak));
extern unsigned long __libcrunch_is_a_hit_cache __attribute__((weak));

extern unsigned int /* __thread */ __libcrunch_is_a_cache_validity;
extern const unsigned short __libcrunch_is_a_cache_size;
extern unsigned short __libcrunch_is_a_cache_next_victim;
#ifndef LIBCRUNCH_MAX_IS_A_CACHE_SIZE
#define LIBCRUNCH_MAX_IS_A_CACHE_SIZE 4
#endif
struct __libcrunch_is_a_cache_s
{
	const void *obj;
	unsigned long long uniqtype:((8 * sizeof(void*))-1);
	unsigned result:1;
	unsigned short period;
	unsigned short n_pos;
	unsigned short n_neg;
	/* add alloc base and uniqtype, to do inline uniqtype cache word check? */
};
struct __libcrunch_is_a_cache_s /* __thread */ __libcrunch_is_a_cache[LIBCRUNCH_MAX_IS_A_CACHE_SIZE] __attribute__((weak)); /* some length */

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
	if (__builtin_expect(! & __libcrunch_is_initialized, 0))
	{
		/* This means that we're not linked with libcrunch. 
		 * There's nothing we can do! */
		return -1;
	}
	if (__builtin_expect(!__libcrunch_is_initialized, 0))
	{
		/* This means we haven't initialized.
		 * Try that now (it won't try more than once). */
		int ret = __libcrunch_global_init ();
		return ret;
	}
	
	return 0;
}

/* FIXME: reinstate "extended counts" versions, which were

#ifdef LIBCRUNCH_EXTENDED_COUNTS
#define LIBCRUNCH_BASIC_CHECKS \
	do { \
		++__libcrunch_begun; \
		// Check for init first, else we can't use the counts. \
		\ 
		if (__builtin_expect((__libcrunch_check_init() == -1), 0)) \
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
		if (__builtin_expect((__libcrunch_check_init() == -1), 0)) \
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
	if (__builtin_expect(r == NULL, 0))
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

#ifdef LIBCRUNCH_TRACE_WIDEN_INT_TO_POINTER
void warnx(const char *fmt, ...);
static void * (__attribute__((noinline)) get_pc)(void);
static void * (__attribute__((noinline)) get_pc)(void)
{
	return __builtin_return_address(0);
}
#endif

extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val, unsigned long from_size);
extern inline void (__attribute__((always_inline,gnu_inline)) __libcrunch_trace_widen_int_to_pointer )(unsigned long long val, unsigned long from_size)
{
#ifdef LIBCRUNCH_TRACE_WIDEN_INT_TO_POINTER
	/* To get a return address, use a noinline nested function. */
	if (from_size < sizeof (void*)) warnx("Unsafe integer-to-pointer cast of value %llx %at %p\n", val, get_pc());
#endif
}

extern inline int (__attribute__((always_inline,gnu_inline)) __is_aU )(const void *obj, const void *uniqtype)
{
	if (!obj) 
	{ 
		return 1; 
	} 
	if (obj == (void*) -1) 
	{ 
		return 1; 
	} 
	// int inited = __libcrunch_check_init (); 
	// if (__builtin_expect((inited == -1), 0)) 
	// { 
	//	 return 1; 
	// } 
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */ 
	if (__builtin_expect( !uniqtype, 0)) 
	{ 
	   __libcrunch_begun++; 
	   __libcrunch_aborted_typestr++; 
		 return 1; 
	} 
	/* No need for the char check in the CIL version */ 
	// now we're really started 
	__libcrunch_begun++; 

	unsigned i;
	for (i = 0; i < __libcrunch_is_a_cache_size; ++i)
	{
		if (__libcrunch_is_a_cache_validity & (1<<i))
		{
			unsigned long long cache_uniqtype = __libcrunch_is_a_cache[i].uniqtype;
			/* We test whether the difference is divisible by the period and within the bounds n_pos and n_neg */
			signed long long diff = (char*) __libcrunch_is_a_cache[i].obj - (char*) obj;
			if (
				(diff == 0
					|| (diff < 0 /* cache obj starts after obj */
						&& __libcrunch_is_a_cache[i].period != 0
						&& diff % __libcrunch_is_a_cache[i].period == 0
						&& -diff / __libcrunch_is_a_cache[i].period <= __libcrunch_is_a_cache[i].n_neg)
					|| (diff > 0
						&& __libcrunch_is_a_cache[i].period != 0
						&& diff % __libcrunch_is_a_cache[i].period == 0
						&& diff / __libcrunch_is_a_cache[i].period >= __libcrunch_is_a_cache[i].n_pos)
				)
				&& (void*) cache_uniqtype == uniqtype
			)
			{
				// hit!
				// - make sure we're not the next victim
				if (__builtin_expect(__libcrunch_is_a_cache_next_victim == i, 0))
				{
					if (__libcrunch_is_a_cache_size > 0)
					{
						__libcrunch_is_a_cache_next_victim
						 = (__libcrunch_is_a_cache_next_victim + 1) % __libcrunch_is_a_cache_size;
					}
				}
				// return the result
				++__libcrunch_is_a_hit_cache;
				if (__libcrunch_is_a_cache[i].result) 
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
		}
	}
	// miss: __is_a_internal will cache if it's cacheable
	int ret = __is_a_internal(obj, uniqtype); 
	
	return ret;
}

extern inline int (__attribute__((always_inline,gnu_inline)) __is_aS) (const void *obj, const char *typestr);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_aS) (const void *obj, const char *typestr)
{
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init ();
	// if (__builtin_expect((inited == -1), 0))
	// {
	//	 return 1;
	// }

	const void * r = __libcrunch_typestr_to_uniqtype(typestr);

	int ret = __is_aU(obj, r);
	return ret;

}

extern inline int (__attribute__((always_inline,gnu_inline)) __like_aU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __like_aU )(const void *obj, const void *uniqtype)
{
	if (!obj) 
	{ 
		return 1; 
	} 
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init (); 
	// if (__builtin_expect((inited == -1), 0)) 
	// { 
	//	 return 1; 
	// } 
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */ 
	if (__builtin_expect( !uniqtype, 0)) 
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
}

extern inline int (__attribute__((always_inline,gnu_inline)) __named_aU )(const void *obj, const char *s);
extern inline int (__attribute__((always_inline,gnu_inline)) __named_aU )(const void *obj, const char *s)
{
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init ();
	// if (/*__builtin_expect(*/(inited == -1)/*, 0)*/)
	// {
	//	 return 1;
	// }

	/* Null uniqtype means __is_aS got a bad typestring, OR we're not  
	 * linked with enough uniqtypes data. */
	if (/*__builtin_expect(*/ !s/*, 0)*/)
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
}
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_function_refiningU )(const void *obj, const void *uniqtype);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_function_refiningU )(const void *obj, const void *uniqtype)
{
	if (!obj)
	{
		return 1;
	}
	if (obj == (void*) -1)
	{
		return 1;
	}
	// int inited = __libcrunch_check_init ();
	// if (__builtin_expect((inited == -1), 0))
	// {
	//	 return 1;
	// }
	
	/* Null uniqtype means __is_aS got a bad typestring, OR we're not 
	 * linked with enough uniqtypes data. */
	if (__builtin_expect( !uniqtype, 0))
	{
		__libcrunch_begun++;
		__libcrunch_aborted_typestr++;
		return 1;
	}
	// now we're really started
	__libcrunch_begun++;
	int ret = __is_a_function_refining_internal(obj, uniqtype);
	return ret;
}
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_pointer_of_degree)(const void *obj, int d);
extern inline int (__attribute__((always_inline,gnu_inline)) __is_a_pointer_of_degree)(const void *obj, int d)
{
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
}
extern inline int (__attribute__((always_inline,gnu_inline)) __can_hold_pointer)(const void *target, const void *value);
extern inline int (__attribute__((always_inline,gnu_inline)) __can_hold_pointer)(const void *target, const void *value)
{
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
}

#endif
