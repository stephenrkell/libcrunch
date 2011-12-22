#ifndef LIBCRUNCH_H_
#define LIBCRUNCH_H_

/* x86_64 only, for now */
#if !defined(__x86_64__) && !defined(X86_64)
#error Unsupported architecture.
#endif

#include "memtable.h"
#include "heap_index.h"

extern void *__uniqtypes_handle __attribute__((weak));
 
/* Copied from uniqtypes.cpp */
struct rec
{
	const char *name;
	unsigned sz;
	unsigned len;
	struct { 
		unsigned offset;
		struct rec *ptr;
	} contained[];
};
struct allocsite_entry
{ 
	void *next; 
	void *prev;
	void *allocsite; 
	struct rec *uniqtype;
};

#define STACK_BEGIN 0x800000000000UL

#define allocsmt_entry_type          struct allocsite_entry *
#define allocsmt_entry_coverage      256
extern allocsmt_entry_type *allocsmt;
#define ALLOCSMT_FUN(op, ...)    MEMTABLE_ ## op ## _WITH_TYPE(allocsmt, allocsmt_entry_type, \
    allocsmt_entry_coverage, (void*)0, (void*)STACK_BEGIN, ## __VA_ARGS__ )

extern _Bool allocsmt_initialized;
void init_allocsites_memtable(void);

// slow(er) path
struct rec *typestr_to_uniqtype(const char *typestr);

inline struct rec *allocsite_to_uniqtype(const void *allocsite)
{
	if (!allocsmt_initialized) init_allocsites_memtable();
	struct allocsite_entry **bucketpos = ALLOCSMT_FUN(ADDR, allocsite);
	struct allocsite_entry *bucket = *bucketpos;
	for (struct allocsite_entry *p = bucket; p; p = p->next)
	{
		if (p->allocsite == allocsite) return p->uniqtype;
	}
}

/* avoid dependency on libc headers (in this header only) */
void __assert_fail(const char *assertion, 
	const char *file, unsigned int line, const char *function);
void warnx(const char *fmt, ...);
extern unsigned long end; // NOTE: man page just uses "extern end", meaning "int"!
unsigned long malloc_usable_size (void *ptr);

/* our own private assert */
static inline void __libcrunch_private_assert(_Bool cond, const char *reason, 
	const char *f, unsigned l, const char *fn)
{
	if (!cond) __assert_fail(reason, f, l, fn);
}

enum object_memory_kind
{
	UNKNOWN,
	STATIC,
	STACK,
	HEAP
};
/* HACK: on my system, shared libraries are always loaded at the top,
 * from 0x7eff00000000....
 * EXCEPT when we run ldd from a Makefile running dash, in which case
 * they show up at 0x2aaaa00000000+delta, which is weird. I should really
 * check the source of ld-linux.so, but for now, go with the lower addr. */
#define SHARED_LIBRARY_MIN_ADDRESS 0x2aaa00000000UL
inline enum object_memory_kind get_object_memory_kind(const void *obj)
{
	/* For x86-64, we do this in a rough-and-ready way. 
	 * In particular, SHARED_LIBRARY_MIN_ADDRESS is not guaranteed. 
	 * However, we can detect violations of this statically using our ldd output. */
	
	/* We use gcc __builtin_expect to hint that heap is the likely case. */ 
	
	unsigned long addr = (unsigned long) obj;
	
	/* If the address is below the end of the program BSS, it's static. */
	if (__builtin_expect(addr < end, 0)) return STATIC;
	
	/* If the address is greater than RSP and less than top-of-stack,
	 * it's stack. */
	unsigned long current_sp;
	__asm__("movq %%rsp, %0\n" : "=r"(current_sp));
	if (__builtin_expect(addr >= current_sp && addr < STACK_BEGIN, 0)) return STACK;
	
	/* It's between HEAP and STATIC. HACK: use SHARED_LIBRARY_MIN_ADDRESS. */
	if (__builtin_expect(addr >= SHARED_LIBRARY_MIN_ADDRESS, 0)) return STATIC;
	
	return HEAP;
}

// slow-path initialization handler
int initialize_handle(void);

inline int __libcrunch_check_init(void)
{
	if (__builtin_expect(!&__uniqtypes_handle, 0))
	{
		/* This means that we're not linked with libcrunch. 
		 * There's nothing we can do! */
		return -1;
	}
	if (__builtin_expect(!__uniqtypes_handle, 0))
	{
		/* This means we haven't opened the uniqtypes library. 
		 * Try that now (it won't try more than once). */
		return initialize_handle();
	}
	
	return 0;
}

// forward decl
inline int __is_aU(const void *obj, const struct rec *uniqtype);

/* Version based on dlsym(). We make this *non*-inline because we want to
   instantiate it in libcrunch.c. */
inline int __is_a(const void *obj, const char *typestr)
{
	/* All we do is convert typestr to a uniqtype. For this, we need to
	 * ensure we have a handle on the uniqtypes library. We do this 
	 * initialization on a slow path here, rather than in a static
	 * constructor, because this code is supposed to be inlineable.
	 * Adding one constructor per object file would be a waste, with 
	 * potential for nontrivial startup time cost. */
	if (__libcrunch_check_init() == -1) goto abort;
	
	/* Now we have a working handle with which to lookup uniqued types, */
	/* we can look up the uniqtype by name. */
	struct rec *r = typestr_to_uniqtype(typestr);
	
	/* Now delegate. */
	return __is_aU(obj, r);

	__assert_fail("unreachable", __FILE__, __LINE__, __func__);
abort:
	warnx("Aborted __is_a(%p, %p) at %p, reason: %s\n", obj, r, 
		&&abort /* we are inlined, right? */, "init failure");
	return 1;
}

/* Optimised version, for when you already know the uniqtype address. */
int __is_aU(const void *obj, const struct rec *uniqtype)
{
	const char *reason = NULL; // if we abort, set this to a string lit
	_Bool suppress_warning = 0;
	
	/* A null pointer always satisfies is_a. */
	if (!obj) return 1;
	
	/* It's okay to assume we're inited, otherwise how did the caller
	 * get the uniqtype in the first place? */
	
	/* To get the uniqtype for obj, we need to determine its memory
	 * location. x86-64 only! */
	void *object_start;
	unsigned offset;
	unsigned block_element_count = 1;
	struct rec *obj_uniqtype = (struct rec *)0;
	switch(get_object_memory_kind(obj))
	{
		case STACK:
		{
			reason = "stack object";
			suppress_warning = 1;
			goto abort;
			//void *uniqtype = stack_frame_to_uniqtype(frame_base, file_relative_ip);
		}
		case HEAP:
		{
			/* For heap allocations, we look up the allocation site.
			 * (This also yields an offset within a toplevel object.)
			 * Then we translate the allocation site to a uniqtypes rec location.
			 * (For direct calls in eagerly-loaded code, we can cache this information
			 * within uniqtypes itself. How? Make uniqtypes include a hash table with
			 * initial contents mapping allocsites to uniqtype recs. This hash table
			 * is initialized during load, but can be extended as new allocsites
			 * are discovered, e.g. indirect ones.)
			 */			
			struct trailer *heap_info = lookup_object_info(obj, &object_start);
			reason = "unindexed heap object";
			if (!heap_info) goto abort;

			// now we have an allocsite
			obj_uniqtype = allocsite_to_uniqtype(heap_info->alloc_site);
			if (!obj_uniqtype) 
			{
				reason = "unrecognised allocsite";
				goto abort;
			}
			unsigned chunk_size = malloc_usable_size(object_start);
			block_element_count = chunk_size / obj_uniqtype->sz;
			__libcrunch_private_assert(chunk_size % obj_uniqtype->sz == 0,
				"chunk size should be multiple of element size", __FILE__, __LINE__, __func__);
		}
		case STATIC:
		{
			//void *uniqtype = static_obj_to_uniqtype(object_start);
			reason = "static object";
			goto abort;
		}
		case UNKNOWN:
		default:
		{
			reason = "unknown object storage";
			goto abort;
		}
	}
	
	/* Now search iteratively for a match at the offset within the toplevel
	 * object. Nonzero offsets "recurse" immediately, using binary search. */
	unsigned target_offset = ((char*) object_start - (char*) obj) % uniqtype->sz;

	signed descend_to_ind;
	do
	{
		/* If we have offset == 0, we can check at this uniqtype. */
		if (obj_uniqtype == uniqtype) return 1;
	
		/* calculate the offset to descend to, if any */
		unsigned num_contained = uniqtype->len;
		unsigned lower_ind = 0;
		unsigned upper_ind = num_contained;
		while (lower_ind < (upper_ind - 1))
		{
			/* Bisect the interval */
			unsigned bisect_ind = (upper_ind - lower_ind) / 2;
			if (uniqtype->contained[bisect_ind].offset > target_offset)
			{
				/* Our solution lies in the lower half of the interval */
				upper_ind = bisect_ind + 1;
			} else lower_ind = bisect_ind;
		}
		if (lower_ind == (upper_ind - 1))
		{
			/* We found one offset */
			__libcrunch_private_assert(uniqtype->contained[lower_ind].offset < target_offset,
				"offset underappoximates", __FILE__, __LINE__, __func__);
			descend_to_ind = lower_ind;
		}
		else /* lower_ind > upper_ind */
		{
			// this should mean num_contained == 0
			__libcrunch_private_assert(num_contained == 0,
				"no contained objects", __FILE__, __LINE__, __func__);
			descend_to_ind = -1;
		}
		
		/* Terminate or recurse. */
	} while (descend_to_ind != -1 
	    && (uniqtype = uniqtype->contained[descend_to_ind].ptr,
	        target_offset = target_offset - uniqtype->contained[descend_to_ind].offset,
	        1));
	// if we got here, the check failed
	goto check_failed;
	
	__assert_fail("unreachable", __FILE__, __LINE__, __func__);
check_failed:
	warnx("Failed check __is_aU(%p, %p) at %p\n", obj, uniqtype, &&check_failed /* we are inlined, right? */);
	return 1;

abort:
	if (!suppress_warning) warnx("Aborted __is_aU(%p, %p) at %p, reason: %s\n", obj, uniqtype,
		&&abort /* we are inlined, right? */, reason);
	return 1;
}

#endif
