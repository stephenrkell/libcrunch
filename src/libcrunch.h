#ifndef LIBCRUNCH_H_
#define LIBCRUNCH_H_

/* x86_64 only, for now */
#if !defined(__x86_64__) && !defined(X86_64)
#error Unsupported architecture.
#endif

#include "memtable.h"
#include "heap_index.h"
#include <stdint.h>
#include "addrmap.h"

extern void *__uniqtypes_handle __attribute__((weak));
 
/* Copied from uniqtypes.cpp */
struct rec
{
	const char *name;
	unsigned sz;
	unsigned len;
	struct { 
		signed offset;
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

#define allocsmt_entry_type          struct allocsite_entry *
#define allocsmt_entry_coverage      256
extern allocsmt_entry_type *allocsmt;
#define ALLOCSMT_FUN(op, ...)    (MEMTABLE_ ## op ## _WITH_TYPE(allocsmt, allocsmt_entry_type, \
    allocsmt_entry_coverage, (void*)0, (void*)STACK_BEGIN, ## __VA_ARGS__ ))

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
unsigned long malloc_usable_size (void *ptr);
int strcmp(const char *, const char *);

/* our own private assert */
static inline void __libcrunch_private_assert(_Bool cond, const char *reason, 
	const char *f, unsigned l, const char *fn)
{
	if (!cond) __assert_fail(reason, f, l, fn);
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

extern const struct rec *__libcrunch_uniqtype_void;
/* counters */
extern unsigned long __libcrunch_begun;
extern unsigned long __libcrunch_aborted_init;
extern unsigned long __libcrunch_aborted_stack;
extern unsigned long __libcrunch_aborted_static;
extern unsigned long __libcrunch_aborted_unknown_storage;
extern unsigned long __libcrunch_aborted_unindexed_heap;
extern unsigned long __libcrunch_aborted_unrecognised_allocsite;
extern unsigned long __libcrunch_failed;
extern unsigned long __libcrunch_trivially_succeeded_null;
extern unsigned long __libcrunch_trivially_succeeded_void;
extern unsigned long __libcrunch_succeeded;

/** This version lets the caller supply a local (cached) location 
 * where the uniqtype will be stored if set. */
inline int __is_a3(const void *obj, const char *typestr, const struct rec **maybe_uniqtype)
{
	/* All we do is convert typestr to a uniqtype. For this, we need to
	 * ensure we have a handle on the uniqtypes library. We do this 
	 * initialization on a slow path here, rather than in a static
	 * constructor, because this code is supposed to be inlineable.
	 * Adding one constructor per object file would be a waste, with 
	 * potential for nontrivial startup time cost. */
	const struct rec *r = NULL;
	if (__libcrunch_check_init() == -1) goto abort;
	
	/* Now we have a working handle with which to lookup uniqued types, */
	/* we can look up the uniqtype by name. */
	if (maybe_uniqtype)
	{
		/* the uniqtype may be cached there... */
		if (*maybe_uniqtype)
		{
			r = *maybe_uniqtype;
		}
		else // not cached
		{
			r = *maybe_uniqtype = typestr_to_uniqtype(typestr);
		}
	}
	else r = typestr_to_uniqtype(typestr);
	
	// if we get a null result, it means we got a typestring that
	// doesn't denote any type that was reified as a uniqtype. In turn, 
	// this might be because it wasn't heap-allocated (or static- or
	// stack-, once I get round to it) by any allocation that we've seen. 
	// BUT if there is some allocation that we didn't see, 
	// OR if we're not doing is_a matching, but, say "__like_a" on some
	// type that is never instantiated (i.e. only structually-equiv ones are),
	// this might arise. 
	// We should handle this by dumping some extra information from trumptr,
	// listing all the typestrings that are actually used, so that we can
	// ensure they all have uniqtypes. For now, we will just see how it goes....
	
	/* Now delegate. */
	assert(r);
	return __is_aU(obj, r);

	__assert_fail("unreachable", __FILE__, __LINE__, __func__);
abort:
	++__libcrunch_begun;
	++__libcrunch_aborted_init;
	warnx("Aborted __is_a(%p, %p) at %p, reason: %s\n", obj, r, 
		&&abort /* we are inlined, right? */, "init failure");
	return 1;

}

/* Version based on dlsym(). */
inline int __is_a(const void *obj, const char *typestr)
{
	return __is_a3(obj, typestr, (void*)0);
}

/* Optimised version, for when you already know the uniqtype address. */
int __is_aU(const void *obj, const struct rec *test_uniqtype)
{
	const char *reason = NULL; // if we abort, set this to a string lit
	const void *reason_ptr = NULL; // if we abort, set this to a useful address
	_Bool suppress_warning = 0;
	
	// NOTE: we handle separately the path where __is_a fails because of init failure
	++__libcrunch_begun;
	
	/* A null pointer always satisfies is_a. */
	if (!obj) { ++__libcrunch_trivially_succeeded_null; return 1; }
	/* Any pointer satisfies void. We do this both here and in typestr_to_uniqtype,
	 * in case we're not called through the __is_a typestr-based interface. */
	if (!__libcrunch_uniqtype_void)
	{
		if (strcmp(test_uniqtype->name, "void") == 0)
		{
			__libcrunch_uniqtype_void = test_uniqtype;
		}
	}
	if (__libcrunch_uniqtype_void && test_uniqtype == __libcrunch_uniqtype_void)
	{ ++__libcrunch_trivially_succeeded_void; return 1; }
	
	/* It's okay to assume we're inited, otherwise how did the caller
	 * get the uniqtype in the first place? */
	
	/* To get the uniqtype for obj, we need to determine its memory
	 * location. x86-64 only! */
	void *object_start;
	unsigned offset;
	unsigned block_element_count = 1;
	struct rec *alloc_uniqtype = (struct rec *)0;

/* HACK: pasted from heap.cpp in libpmirror */
/* Do I want to pad to 4, 8 or (=== 4 (mod 8)) bytes? 
 * Try 4 mod 8. */
#define PAD_TO_NBYTES(s, n) (((s) % (n) == 0) ? (s) : ((((s) / (n)) + 1) * (n)))
#define PAD_TO_MBYTES_MOD_N(s, n, m) (((s) % (n) <= (m)) \
? ((((s) / (n)) * (n)) + (m)) \
: (((((s) / (n)) + 1) * (n)) + (m)))
#define USABLE_SIZE_FROM_OBJECT_SIZE(s) (PAD_TO_MBYTES_MOD_N( ((s) + sizeof (struct trailer)) , 8, 4))
#define HEAPSZ_ONE(t) (USABLE_SIZE_FROM_OBJECT_SIZE(sizeof ((t))))

	switch(get_object_memory_kind(obj))
	{
		case STACK:
		{
			reason = "stack object";
			reason_ptr = obj;
			suppress_warning = 1;
			++__libcrunch_aborted_stack;
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
			if (!heap_info)
			{
				reason = "unindexed heap object";
				reason_ptr = obj;
				++__libcrunch_aborted_unindexed_heap;
				goto abort;
			}

			// now we have an allocsite
			alloc_uniqtype = allocsite_to_uniqtype(heap_info->alloc_site);
			if (!alloc_uniqtype) 
			{
				reason = "unrecognised allocsite";
				reason_ptr = heap_info->alloc_site;
				++__libcrunch_aborted_unrecognised_allocsite;
				goto abort;
			}
			unsigned chunk_size = malloc_usable_size(object_start);
			unsigned padded_trailer_size = USABLE_SIZE_FROM_OBJECT_SIZE(0);
			block_element_count = (chunk_size - padded_trailer_size) / alloc_uniqtype->sz;
			//__libcrunch_private_assert(chunk_size % alloc_uniqtype->sz == 0,
			//	"chunk size should be multiple of element size", __FILE__, __LINE__, __func__);
			break;
		}
		case STATIC:
		{
			//void *uniqtype = static_obj_to_uniqtype(object_start);
			reason = "static object";
			reason_ptr = obj;
			++__libcrunch_aborted_static;
			goto abort;
		}
		case UNKNOWN:
		default:
		{
			reason = "object of unknown storage";
			reason_ptr = obj;
			++__libcrunch_aborted_unknown_storage;
			goto abort;
		}
	}
		
	/* Now search iteratively for a match at the offset within the toplevel
	 * object. Nonzero offsets "recurse" immediately, using binary search. */
	assert(alloc_uniqtype);
	unsigned target_offset = ((char*) obj - (char*) object_start) % 
		(alloc_uniqtype->sz ? alloc_uniqtype->sz : 1);
	
	struct rec *cur_obj_uniqtype = alloc_uniqtype;
	signed descend_to_ind;
	do
	{
		/* If we have offset == 0, we can check at this uniqtype. */
		if (cur_obj_uniqtype == test_uniqtype) 
		{
		temp_label: // HACK: remove this printout once stable
			warnx("Check __is_aU(%p, %p a.k.a. \"%s\") succeeded at %p.\n", 
				obj, test_uniqtype, test_uniqtype->name, &&temp_label);
			++__libcrunch_succeeded;
			return 1;
		}
	
		/* calculate the offset to descend to, if any 
		 * FIXME: refactor into find_subobject_spanning(offset) */
		unsigned num_contained = cur_obj_uniqtype->len;
		int lower_ind = 0;
		int upper_ind = num_contained;
		while (lower_ind + 1 < upper_ind) // difference of >= 2
		{
			/* Bisect the interval */
			int bisect_ind = (upper_ind + lower_ind) / 2;
			__libcrunch_private_assert(bisect_ind > lower_ind, "bisection progress", 
				__FILE__, __LINE__, __func__);
			if (cur_obj_uniqtype->contained[bisect_ind].offset > target_offset)
			{
				/* Our solution lies in the lower half of the interval */
				upper_ind = bisect_ind;
			} else lower_ind = bisect_ind;
		}
		if (lower_ind + 1 == upper_ind)
		{
			/* We found one offset */
			__libcrunch_private_assert(cur_obj_uniqtype->contained[lower_ind].offset <= target_offset,
				"offset underappoximates", __FILE__, __LINE__, __func__);
			descend_to_ind = lower_ind;
		}
		else /* lower_ind >= upper_ind */
		{
			// this should mean num_contained == 0
			__libcrunch_private_assert(num_contained == 0,
				"no contained objects", __FILE__, __LINE__, __func__);
			descend_to_ind = -1;
		}
		
		/* Terminate or recurse. */
	} while (descend_to_ind != -1 
	    && (cur_obj_uniqtype = cur_obj_uniqtype->contained[descend_to_ind].ptr,
	        target_offset = target_offset - cur_obj_uniqtype->contained[descend_to_ind].offset,
	        1));
	// if we got here, the check failed
	goto check_failed;
	
	__assert_fail("unreachable", __FILE__, __LINE__, __func__);
check_failed:
	++__libcrunch_failed;
	warnx("Failed check __is_aU(%p, %p a.k.a. \"%s\") at %p, allocation was a %p (a.k.a. \"%s\")\n", 
		obj, test_uniqtype, test_uniqtype->name,
		&&check_failed /* we are inlined, right? */,
		alloc_uniqtype, alloc_uniqtype->name);
	return 1;

abort:
	// if (!suppress_warning) warnx("Aborted __is_aU(%p, %p) at %p, reason: %s (%p)\n", obj, uniqtype,
	//	&&abort /* we are inlined, right? */, reason, reason_ptr); 
	return 1; // so that the program will continue
}

#endif
