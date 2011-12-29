#include <stdlib.h>

extern __thread void *__current_allocsite __attribute__((weak)); // defined by heap_index_hooks

/* these are our per-allocfn wrappers */

// xcalloc(zZ)

extern void *__real_xcalloc(size_t nmemb, size_t size) __attribute__((weak));
void *__wrap_xcalloc(size_t nmemb, size_t size)
{
	if (&__current_allocsite && !__current_allocsite)
	{
		__current_allocsite = __builtin_return_address(0);
		void *retval = __real_xcalloc(nmemb, size);
		__current_allocsite = (void*)0;
		return retval;
	}
	else return __real_xcalloc(nmemb, size);
}

// xmalloc(Z)
extern void *__real_xmalloc(size_t size) __attribute__((weak));
void *__wrap_xmalloc(size_t size)
{
	if (&__current_allocsite && !__current_allocsite)
	{
		__current_allocsite = __builtin_return_address(0);
		void *retval = __real_xmalloc(size);
		__current_allocsite = (void*)0;
		return retval;
	}
	else return __real_xmalloc(size);
}

// xrealloc(pZ) 
extern void *__real_xrealloc(void *p, size_t size) __attribute__((weak));
void *__wrap_xrealloc(void *p, size_t size)
{
	if (&__current_allocsite && !__current_allocsite)
	{
		__current_allocsite = __builtin_return_address(0);
		void *retval = __real_xrealloc(p, size);
		__current_allocsite = (void*)0;
		return retval;
	}
	else return __real_xrealloc(p, size);
}

// xmallocz(Z)
extern void *__real_xmallocz(size_t size) __attribute__((weak));
void *__wrap_xmallocz(size_t size)
{
	if (&__current_allocsite && !__current_allocsite)
	{
		__current_allocsite = __builtin_return_address(0);
		void *retval = __real_xmallocz(size);
		__current_allocsite = (void*)0;
		return retval;
	}
	else return __real_xmallocz(size);
}
