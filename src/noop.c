static const int __libcrunch_is_initialized = 1;

unsigned int __libcrunch_is_a_cache_validity; // all zeroes
unsigned short __libcrunch_is_a_cache_next_victim;
const unsigned short __libcrunch_is_a_cache_size; // zero
unsigned long int __libcrunch_begun = 0;
unsigned long int __libcrunch_failed = 0;
unsigned long int __libcrunch_succeeded = 0;
unsigned long int __libcrunch_aborted_typestr = 0;
unsigned long int __libcrunch_is_a_hit_cache = 0;
unsigned long int __libcrunch_created_invalid_pointer = 0;
unsigned long int __libcrunch_checked_pointer_adjustments = 0;
unsigned long int __libcrunch_fetch_bounds_called = 0;

/* We really want to fit in 64 bits on x86-64. */
struct __libcrunch_bounds_s
{
	void *base;
	void *limit;
};
typedef struct __libcrunch_bounds_s __libcrunch_bounds_t;

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

__libcrunch_bounds_t __fetch_bounds_internal(const void *ptr, const void *t)
{
        return (__libcrunch_bounds_t) { (void*) 0, (void*) -1 };
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
