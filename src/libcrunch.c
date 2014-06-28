/* Libcrunch contains all the non-inline code that we need for doing run-time 
 * type checks on C code. */

#define _GNU_SOURCE

#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <link.h>
#include <sys/time.h>
#include <sys/resource.h>
#ifndef USE_FAKE_LIBUNWIND
#include <libunwind.h>
#endif
#include "libcrunch.h"
#include "libcrunch_private.h"

int __libcrunch_debug_level;
_Bool __libcrunch_is_initialized;

const char *exe_basename __attribute__((visibility("hidden")));
FILE *stream_err __attribute__((visibility("hidden")));

// helper
static const void *typestr_to_uniqtype_from_lib(void *handle, const char *typestr);

// HACK
void __libcrunch_preload_init(void);

/* Some data types like void* and sockaddr appear to be used to size a malloc(), 
 * but are only used because they have the same size as the actual thing being
 * allocated (say a different type of pointer, or a family-specific sockaddr). 
 * We keep a list of these. The user can use the LIBCRUNCH_LAZY_HEAP_TYPES 
 * environment variable to add these. */
static unsigned lazy_heap_types_count;
static const char **lazy_heap_typenames;
static struct uniqtype **lazy_heap_types;

static int print_type_cb(struct uniqtype *t, void *ignored)
{
	fprintf(stream_err, "uniqtype addr %p, name %s, size %d bytes\n", 
		t, t->name, t->pos_maxoff);
	fflush(stream_err);
	return 0;
}

static int match_typename_cb(struct uniqtype *t, void *ignored)
{
	for (unsigned i = 0; i < lazy_heap_types_count; ++i)
	{
		if (!lazy_heap_types[i] && 
			0 == strcmp(t->name, lazy_heap_typenames[i]))
		{
			// install this type in the lazy_heap_type slot
			lazy_heap_types[i] = t;
			
			// keep going -- we might have more to match
			return 0;
		}
	}
	return 0; // keep going
}

void __libcrunch_scan_lazy_typenames(void *typelib_handle)
{
	__liballocs_iterate_types(typelib_handle, match_typename_cb, NULL);

	// for (unsigned i = 0; i < lazy_heap_types_count; ++i)
	// {
	// 	if (lazy_heap_typenames[i] && !lazy_heap_types[i])
	// 	{
	// 		// look up 
	// 		const void *u = typestr_to_uniqtype_from_lib(typelib_handle, lazy_heap_typenames[i]);
	// 		// if we found it, install it
	// 		if (u) lazy_heap_types[i] = (struct uniqtype *) u;
	//	}
	// }
}

static ElfW(Dyn) *get_dynamic_section(void *handle)
{
	return ((struct link_map *) handle)->l_ld;
}

static ElfW(Dyn) *get_dynamic_entry_from_section(void *dynsec, unsigned long tag)
{
	ElfW(Dyn) *dynamic_section = dynsec;
	while (dynamic_section->d_tag != DT_NULL
		&& dynamic_section->d_tag != tag) ++dynamic_section;
	if (dynamic_section->d_tag == DT_NULL) return NULL;
	return dynamic_section;
}

static ElfW(Dyn) *get_dynamic_entry_from_handle(void *handle, unsigned long tag)
{
	return get_dynamic_entry_from_section(((struct link_map *) handle)->l_ld, tag);
}

static _Bool is_lazy_uniqtype(const void *u)
{
	for (unsigned i = 0; i < lazy_heap_types_count; ++i)
	{
		if (lazy_heap_types[i] == u) return 1;
	}
	return 0;
}

static _Bool done_init;
void __libcrunch_main_init(void) __attribute__((constructor(101)));
// NOTE: runs *before* the constructor in preload.c
void __libcrunch_main_init(void)
{
	assert(!done_init);
	
	done_init = 1;
}

const struct uniqtype *__libcrunch_uniqtype_void; // remember the location of the void uniqtype
const struct uniqtype *__libcrunch_uniqtype_signed_char;
const struct uniqtype *__libcrunch_uniqtype_unsigned_char;
#define LOOKUP_CALLER_TYPE(frag, caller) /* FIXME: use caller not RTLD_DEFAULT -- use interval tree? */ \
    ( \
		(__libcrunch_uniqtype_ ## frag) ? __libcrunch_uniqtype_ ## frag : \
		(__libcrunch_uniqtype_ ## frag = dlsym(RTLD_DEFAULT, "__uniqtype__" #frag), \
			assert(__libcrunch_uniqtype_ ## frag), \
			__libcrunch_uniqtype_ ## frag \
		) \
	)

/* counters */
unsigned long __libcrunch_begun;
#ifdef LIBCRUNCH_EXTENDED_COUNTS
unsigned long __libcrunch_aborted_init;
unsigned long __libcrunch_trivially_succeeded;
#endif
unsigned long __libcrunch_aborted_typestr;
unsigned long __libcrunch_lazy_heap_type_assignment;
unsigned long __libcrunch_failed;
unsigned long __libcrunch_failed_in_alloc;
unsigned long __libcrunch_succeeded;

static unsigned long suppression_count;
enum check
{
	IS_A,
	LIKE_A
};
static enum check last_suppressed_check_kind;

static void print_exit_summary(void)
{
	if (__libcrunch_begun == 0) return;
	
	if (suppression_count > 0)
	{
		debug_printf(0, "Suppressed %ld further occurrences of the previous error\n", 
				suppression_count);
	}
	
	fprintf(stream_err, "====================================================\n");
	fprintf(stream_err, "libcrunch summary: \n");
	fprintf(stream_err, "----------------------------------------------------\n");
	fprintf(stream_err, "checks begun:                              % 9ld\n", __libcrunch_begun);
	fprintf(stream_err, "----------------------------------------------------\n");
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(stream_err, "checks aborted due to init failure:        % 9ld\n", __libcrunch_aborted_init);
#endif
	fprintf(stream_err, "checks aborted for bad typename:           % 9ld\n", __libcrunch_aborted_typestr);
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(stream_err, "checks trivially passed:                   % 9ld\n", __libcrunch_trivially_succeeded);
#endif
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(stream_err, "checks remaining                           % 9ld\n", __libcrunch_begun - (__libcrunch_trivially_succeeded + __liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr + __libcrunch_aborted_init));
#else
	fprintf(stream_err, "checks remaining                           % 9ld\n", __libcrunch_begun - (__liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr));
#endif	
	fprintf(stream_err, "----------------------------------------------------\n");
	fprintf(stream_err, "   of which did lazy heap type assignment: % 9ld\n", __libcrunch_lazy_heap_type_assignment);
	fprintf(stream_err, "----------------------------------------------------\n");
	fprintf(stream_err, "checks failed inside allocation functions: % 9ld\n", __libcrunch_failed_in_alloc);
	fprintf(stream_err, "checks failed otherwise:                   % 9ld\n", __libcrunch_failed);
	fprintf(stream_err, "checks nontrivially passed:                % 9ld\n", __libcrunch_succeeded);
	fprintf(stream_err, "====================================================\n");

	if (getenv("LIBCRUNCH_DUMP_SMAPS_AT_EXIT"))
	{
		char buffer[4096];
		size_t bytes;
		FILE *smaps = fopen("/proc/self/smaps", "r");
		if (smaps)
		{
			while (0 < (bytes = fread(buffer, 1, sizeof(buffer), smaps)))
			{
				fwrite(buffer, 1, bytes, stream_err);
			}
		}
		else fprintf(stream_err, "Couldn't read from smaps!\n");
	}
}

/* This is *not* a constructor. We don't want to be called too early,
 * because it might not be safe to open the -uniqtypes.so handle yet.
 * So, initialize on demand. */
int __libcrunch_global_init(void)
{
	if (__libcrunch_is_initialized) return 0; // we are okay

	// don't try more than once to initialize
	static _Bool tried_to_initialize;
	if (tried_to_initialize) return -1;
	tried_to_initialize = 1;

	// print a summary when the program exits
	atexit(print_exit_summary);
	
	// we must have initialized liballocs
	__liballocs_ensure_init();
	
	// delay start-up here if the user asked for it
	if (getenv("LIBCRUNCH_DELAY_STARTUP"))
	{
		sleep(10);
	}

		// figure out where our output goes
	const char *errvar = getenv("LIBCRUNCH_ERR");
	if (errvar)
	{
		// try opening it
		stream_err = fopen(errvar, "w");
		if (!stream_err)
		{
			stream_err = stderr;
			debug_printf(0, "could not open %s for writing\n", errvar);
		}
	} else stream_err = stderr;
	assert(stream_err);

	// grab the executable's basename
	char exename[4096];
	ssize_t readlink_ret = readlink("/proc/self/exe", exename, sizeof exename);
	if (readlink_ret != -1)
	{
		exe_basename = basename(exename); // GNU basename
	}
	
	const char *debug_level_str = getenv("LIBCRUNCH_DEBUG_LEVEL");
	if (debug_level_str) __libcrunch_debug_level = atoi(debug_level_str);

	/* We always include "signed char" in the lazy heap types (FIXME: this is a 
	 * C-specificity we'd rather not have here, but live with it for now.) 
	 * We count the other ones. */
	const char *lazy_heap_types_str = getenv("LIBCRUNCH_LAZY_HEAP_TYPES");
	unsigned upper_bound = 2; // signed char plus one string with zero spaces
	if (lazy_heap_types_str)
	{
		/* Count the lazy heap types */
		const char *pos = lazy_heap_types_str;
		while ((pos = strrchr(pos, ' ')) != NULL) { ++upper_bound; ++pos; }
	}
	/* Allocate and populate. */
	lazy_heap_typenames = calloc(upper_bound, sizeof (const char *));
	lazy_heap_types = calloc(upper_bound, sizeof (struct uniqtype *));

	// the first entry is always signed char
	lazy_heap_typenames[0] = "signed char";
	lazy_heap_types_count = 1;
	if (lazy_heap_types_str)
	{
		const char *pos = lazy_heap_types_str;
		const char *spacepos;
		do 
		{
			spacepos = strchrnul(pos, ' ');
			if (spacepos - pos > 0) 
			{
				assert(lazy_heap_types_count < upper_bound);
				lazy_heap_typenames[lazy_heap_types_count++] = strndup(pos, spacepos - pos);
			}

			pos = spacepos;
			while (*pos == ' ') ++pos;
		} while (*pos != '\0');
	}
	
	/* We have to scan for lazy heap types *in link order*, so that we see
	 * the first linked definition of any type that is multiply-defined.
	 * Do a scan now; we also scan when loading a types object, and when loading
	 * a user-dlopen()'d object. 
	 * 
	 * We don't use dl_iterate_phdr because it doesn't give us the link_map * itself. 
	 * Instead, walk the link map directly, like a debugger would
	 *                                           (like I always knew somebody should). */
	// grab the executable's end address
	dlerror();
	void *executable_handle = dlopen(NULL, RTLD_NOW | RTLD_NOLOAD);
	assert(executable_handle != NULL);
	void *exec_dynamic = ((struct link_map *) executable_handle)->l_ld;
	assert(exec_dynamic != NULL);
	ElfW(Dyn) *dt_debug = get_dynamic_entry_from_section(exec_dynamic, DT_DEBUG);
	struct r_debug *r_debug = (struct r_debug *) dt_debug->d_un.d_ptr;
	for (struct link_map *l = r_debug->r_map; l; l = l->l_next)
	{
		__libcrunch_scan_lazy_typenames(l);
	}

	__libcrunch_is_initialized = 1;

	debug_printf(1, "libcrunch successfully initialized\n");
	
	return 0;
}

/* Optimised version, for when you already know the uniqtype address. */
int __is_a_internal(const void *obj, const void *arg)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	const char *reason = NULL; // if we abort, set this to a string lit
	const void *reason_ptr = NULL; // if we abort, set this to a useful address
	memory_kind k = UNKNOWN;
	const void *object_start;
	unsigned block_element_count = 1;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;
	
	_Bool abort = __liballocs_get_alloc_info(obj, 
		arg, 
		&reason,
		&reason_ptr,
		&k,
		&object_start,
		&block_element_count,
		&alloc_uniqtype,
		&alloc_site,
		&target_offset_within_uniqtype);
	
	if (__builtin_expect(abort, 0))
	{
		/* If heap classification failed, null out the allocsite 
		 * to avoid repeated searching. We only do this for non-debug
		 * builds because it makes debugging a bit harder.
		 */
		if (__builtin_expect(k == HEAP
				&& reason_ptr != NULL
				&& reason_ptr == alloc_site, 0))
		{
			struct insert *ins = lookup_object_info(obj, NULL, NULL, NULL);
			assert(INSERT_DESCRIBES_OBJECT(ins));
			/* Update the heap chunk's info to null the alloc site. 
			 * PROBLEM: we need to make really sure that we're not nulling
			 * out a redirected (deep) chunk's alloc site. 
			 * 
			 * NOTE that we don't want the insert to look like a deep-index
			 * terminator, so we set the flag.
			 */
			if (ins)
			{
#ifdef NDEBUG
				ins->alloc_site_flag = 1;
				ins->alloc_site = 0;
#endif
				assert(INSERT_DESCRIBES_OBJECT(ins));
				assert(!INSERT_IS_TERMINATOR(ins));
			}
		}
		
		return 1; // liballocs has already counted this abort
	}
	
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;

	signed cumulative_offset_searched = 0;
	_Bool success = __liballocs_find_matching_subobject(target_offset_within_uniqtype, 
			cur_obj_uniqtype, (struct uniqtype *) test_uniqtype, &cur_obj_uniqtype, 
			&target_offset_within_uniqtype, &cumulative_offset_searched);
	
	if (__builtin_expect(success, 1))
	{
		++__libcrunch_succeeded;
		return 1;
	}
	
	// if we got here, we might still need to apply lazy heap typing
	if (__builtin_expect(k == HEAP
			&& alloc_site != NULL // i.e. we haven't yet written a uniqtype ptr into the heap chunk
			&& is_lazy_uniqtype(alloc_uniqtype)
			&& !__currently_allocating, 0))
	{
		++__libcrunch_lazy_heap_type_assignment;
		
		// update the heap chunk's info to say that its type is our test_uniqtype
		struct insert *ins = lookup_object_info(obj, NULL, NULL, NULL);
		assert(ins);
		ins->alloc_site_flag = 1;
		ins->alloc_site = (uintptr_t) test_uniqtype;
		
		return 1;
	}
	
	// if we got here, the check failed
	if (__currently_allocating || __currently_freeing)
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		
		static const void *last_failed_site;
		static const void *last_failed_object_start;
		static const struct uniqtype *last_failed_test_type;
		
		if (last_failed_site == __builtin_return_address(0)
				&& last_failed_object_start == object_start
				&& last_failed_test_type == test_uniqtype
				&& last_suppressed_check_kind == IS_A)
		{
			++suppression_count;
		}
		else
		{
			if (suppression_count > 0)
			{
				debug_printf(0, "Suppressed %ld further occurrences of the previous error\n", 
						suppression_count);
			}
			
			debug_printf(0, "Failed check __is_a_internal(%p, %p a.k.a. \"%s\") at %p, "
					"%ld bytes into an allocation of a %s%s%s "
					"(deepest subobject: %s at offset %d) "
					"originating at %p\n", 
				obj, test_uniqtype, test_uniqtype->name,
				__builtin_return_address(0),
				(long)((char*) obj - (char*) object_start),
				name_for_memory_kind(k), (k == HEAP && block_element_count > 1) ? " block of " : " ", 
				alloc_uniqtype ? (alloc_uniqtype->name ?: "(unnamed type)") : "(unknown type)", 
				(cur_obj_uniqtype ? cur_obj_uniqtype->name : "(none)"), 
					cumulative_offset_searched, 
				alloc_site);
			last_failed_site = __builtin_return_address(0);
			last_failed_object_start = object_start;
			last_failed_test_type = test_uniqtype;
			suppression_count = 0;
		}
	}
	return 1; // HACK: so that the program will continue
}

/* Optimised version, for when you already know the uniqtype address. */
int __like_a_internal(const void *obj, const void *arg)
{
	// FIXME: use our recursive subobject search here? HMM -- semantics are non-obvious.
	
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	const char *reason = NULL; // if we abort, set this to a string lit
	const void *reason_ptr = NULL; // if we abort, set this to a useful address
	memory_kind k;
	const void *object_start;
	unsigned block_element_count = 1;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;
	void *caller_address = __builtin_return_address(0);
	
	_Bool abort = __liballocs_get_alloc_info(obj, 
		arg, 
		&reason,
		&reason_ptr,
		&k,
		&object_start,
		&block_element_count,
		&alloc_uniqtype, 
		&alloc_site,
		&target_offset_within_uniqtype);
	
	if (__builtin_expect(abort, 0)) return 1; // we've already counted it
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;
	
	/* Descend the subobject hierarchy until our target offset is zero, i.e. we 
	 * find the outermost thing in the subobject tree that starts at the address
	 * we were passed (obj). */
	while (target_offset_within_uniqtype != 0)
	{
		_Bool success = __liballocs_first_subobject_spanning(
				&target_offset_within_uniqtype, &cur_obj_uniqtype, &cur_containing_uniqtype,
				&cur_contained_pos);
		if (!success) goto like_a_failed;
	}
	
	// trivially, identical types are like one another
	if (test_uniqtype == cur_obj_uniqtype) goto like_a_succeeded;
	
	// arrays are special
	_Bool matches;
	if (__builtin_expect((cur_obj_uniqtype->is_array || test_uniqtype->is_array), 0))
	{
		matches = 
			test_uniqtype == cur_obj_uniqtype
		||  (test_uniqtype->is_array && test_uniqtype->array_len == 1 
				&& test_uniqtype->contained[0].ptr == cur_obj_uniqtype)
		||  (cur_obj_uniqtype->is_array && cur_obj_uniqtype->array_len == 1
				&& cur_obj_uniqtype->contained[0].ptr == test_uniqtype);
		/* We don't need to allow an array of one blah to be like a different
		 * array of one blah, because they should be the same type. 
		 * FIXME: there's a difficult case: an array of statically unknown length, 
		 * which happens to have length 1. */
		if (matches) goto like_a_succeeded; else goto like_a_failed;
	}
	
	/* If we're not an array and nmemb is zero, we might have base types with
	 * signedness complements. */
	if (!cur_obj_uniqtype->is_array && !test_uniqtype->is_array
			&&  cur_obj_uniqtype->nmemb == 0 && test_uniqtype->nmemb == 0)
	{
		/* Does the cur obj type have a signedness complement matching the test type? */
		if (cur_obj_uniqtype->contained[0].ptr == test_uniqtype) goto like_a_succeeded;
		/* Does the test type have a signedness complement matching the cur obj type? */
		if (test_uniqtype->contained[0].ptr == cur_obj_uniqtype) goto like_a_succeeded;
	}
			
	
	/* Okay, we can start the like-a test: for each element in the test type, 
	 * do we have a type-equivalent in the object type?
	 * 
	 * We make an exception for arrays of char (signed or unsigned): if an
	 * element in the test type is such an array, we skip over any number of
	 * fields in the object type, until we reach the offset of the end element.  */
	unsigned i_obj_subobj = 0, i_test_subobj = 0;
	for (; 
		i_obj_subobj < cur_obj_uniqtype->nmemb && i_test_subobj < test_uniqtype->nmemb; 
		++i_test_subobj, ++i_obj_subobj)
	{
		if (__builtin_expect(test_uniqtype->contained[i_test_subobj].ptr->is_array
			&& (test_uniqtype->contained[i_test_subobj].ptr->contained[0].ptr
					== LOOKUP_CALLER_TYPE(signed_char, caller_address)
			|| test_uniqtype->contained[i_test_subobj].ptr->contained[0].ptr
					== LOOKUP_CALLER_TYPE(unsigned_char, caller_address)), 0))
		{
			// we will skip this field in the test type
			signed target_off =
				test_uniqtype->nmemb > i_test_subobj + 1
			 ?  test_uniqtype->contained[i_test_subobj + 1].offset
			 :  test_uniqtype->contained[i_test_subobj].offset
			      + test_uniqtype->contained[i_test_subobj].ptr->pos_maxoff;
			
			// ... if there's more in the test type, advance i_obj_subobj
			while (i_obj_subobj + 1 < cur_obj_uniqtype->nmemb &&
				cur_obj_uniqtype->contained[i_obj_subobj + 1].offset < target_off) ++i_obj_subobj;
			/* We fail if we ran out of stuff in the target object type
			 * AND there is more to go in the test type. */
			if (i_obj_subobj + 1 >= cur_obj_uniqtype->nmemb
			 && test_uniqtype->nmemb > i_test_subobj + 1) goto like_a_failed;
				
			continue;
		}
		matches = 
				test_uniqtype->contained[i_test_subobj].offset == cur_obj_uniqtype->contained[i_obj_subobj].offset
		 && 	test_uniqtype->contained[i_test_subobj].ptr == cur_obj_uniqtype->contained[i_obj_subobj].ptr;
		if (!matches) goto like_a_failed;
	}
	// if we terminated because we ran out of fields in the target type, fail
	if (i_test_subobj < test_uniqtype->nmemb) goto like_a_failed;
	
like_a_succeeded:
	++__libcrunch_succeeded;
	return 1;
	
	// if we got here, we've failed
	// if we got here, the check failed
like_a_failed:
	if (__currently_allocating || __currently_freeing) 
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		debug_printf(0, "Failed check __like_a_internal(%p, %p a.k.a. \"%s\") at %p, allocation was a %s%s%s originating at %p\n", 
			obj, test_uniqtype, test_uniqtype->name,
			__builtin_return_address(0), // make sure our *caller*, if any, is inlined
			name_for_memory_kind(k), (k == HEAP && block_element_count > 1) ? " block of " : " ", 
			alloc_uniqtype ? (alloc_uniqtype->name ?: "(unnamed type)") : "(unknown type)", 
			alloc_site);
	}
	return 1; // HACK: so that the program will continue
}

int __named_a_internal(const void *obj, const void *arg)
{
	// FIXME: use our recursive subobject search here? HMM -- semantics are non-obvious.
	
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	const char* test_typestr = (const char *) arg;
	const char *reason = NULL; // if we abort, set this to a string lit
	const void *reason_ptr = NULL; // if we abort, set this to a useful address
	memory_kind k;
	const void *object_start;
	unsigned block_element_count = 1;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;
	void *caller_address = __builtin_return_address(0);
	
	_Bool abort = __liballocs_get_alloc_info(obj, 
		arg, 
		&reason,
		&reason_ptr,
		&k,
		&object_start,
		&block_element_count,
		&alloc_uniqtype, 
		&alloc_site,
		&target_offset_within_uniqtype);
	
	if (__builtin_expect(abort, 0)) return 1; // we've already counted it
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;
	signed cumulative_offset_searched = 0;

	/* Look for a matching subobject. */
	_Bool success;
	do 
	{
		_Bool success = __liballocs_find_matching_subobject(target_offset_within_uniqtype, 
			cur_obj_uniqtype, NULL, &cur_obj_uniqtype, 
			&target_offset_within_uniqtype, &cumulative_offset_searched);
		if (__builtin_expect(success, 1))
		{
			/* This means we got a subobject of *some* type. Does it match
			 * the name? */
			// FIXME: cache names
			if 	(0 == strcmp(test_typestr, cur_obj_uniqtype->name)) goto named_a_succeeded;
			else
			{
				/* If we can descend to the first member of this type
				 * and try again, do it.
				 * 
				 * FIXME: it's not the first that matters; it's all zero-offset
				 * members. Ideally we want to refactor find_matching_subobject 
				 * so that it can match by name, but that seems to bring callbacks,
				 * meaning we must be careful not to forestall compiler optimisations. */
				if (cur_obj_uniqtype->nmemb > 0
						&& cur_obj_uniqtype->contained[0].offset == 0)
				{
					cur_obj_uniqtype = cur_obj_uniqtype->contained[0].ptr;
					continue;
				} else goto named_a_failed;
			}
		}
	} while (1);
	

named_a_succeeded:
	++__libcrunch_succeeded;
	return 1;
	
	// if we got here, we've failed
	// if we got here, the check failed
named_a_failed:
	if (__currently_allocating || __currently_freeing) 
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		debug_printf(0, "Failed check __named_a_internal(%p, \"%s\") at %p, allocation was a %s%s%s originating at %p\n", 
			obj, test_typestr,
			__builtin_return_address(0), // make sure our *caller*, if any, is inlined
			name_for_memory_kind(k), (k == HEAP && block_element_count > 1) ? " block of " : " ", 
			alloc_uniqtype ? (alloc_uniqtype->name ?: "(unnamed type)") : "(unknown type)", 
			alloc_site);
	}
	return 1; // HACK: so that the program will continue
}

int 
__check_args_internal(const void *obj, int nargs, ...)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();

	const char *reason = NULL; // if we abort, set this to a string lit
	const void *reason_ptr = NULL; // if we abort, set this to a useful address
	memory_kind k;
	const void *object_start;
	unsigned block_element_count = 1;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;

	struct uniqtype *fun_uniqtype = NULL;
	
	_Bool abort = __liballocs_get_alloc_info(obj, 
		NULL, 
		&reason,
		&reason_ptr,
		&k,
		&object_start,
		&block_element_count,
		&fun_uniqtype,
		&alloc_site,
		&target_offset_within_uniqtype);
	
	assert(fun_uniqtype);
	assert(object_start == obj);
	assert(UNIQTYPE_IS_SUBPROGRAM(fun_uniqtype));
	
	/* Walk the arguments that the function expects. Simultaneously, 
	 * walk our arguments. */
	va_list ap;
	va_start(ap, nargs);
	
	// FIXME: this function screws with the __libcrunch_begun count somehow
	// -- try hello-funptr
	
	_Bool success = 1;
	int i;
	for (i = 0; i < nargs && i < fun_uniqtype->array_len; ++i)
	{
		void *argval = va_arg(ap, void*);
		/* contained[0] is the return type */
		struct uniqtype *expected_arg = fun_uniqtype->contained[i+1].ptr;
		/* We only check casts that are to pointer targets types.
		 * How to test this? */
		if (!expected_arg->is_array && expected_arg->array_len == MAGIC_LENGTH_POINTER)
		{
			struct uniqtype *expected_arg_pointee_type = expected_arg->contained[0].ptr;
			success &= __is_aU(argval, expected_arg_pointee_type);
		}
		if (!success) break;
	}
	if (i == nargs && i < fun_uniqtype->array_len)
	{
		/* This means we exhausted nargs before we got to the end of the array.
		 * In other words, the function takes more arguments than we were passed
		 * for checking, i.e. more arguments than the call site passes. 
		 * Not good! */
		success = 0;
	}
	if (i < nargs && i == fun_uniqtype->array_len)
	{
		/* This means we were passed more args than the uniqtype told us about. 
		 * FIXME: check for its varargs-ness. If it's varargs, we're allowed to
		 * pass more. For now, fail. */
		success = 0;
	}
	
	va_end(ap);
	
	return success ? 0 : i; // 0 means success here
}
