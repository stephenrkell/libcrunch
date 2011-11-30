/* stuff defined by libcrunch */
extern void *__uniqtypes_handle __attribute__((weak));

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
inline struct rec *allocsite_to_uniqtype(void *allocsite)
{
	/* This is a hash- or mem-table lookup. 
	 * Do we want it to be a memtable?
	 * If so, what is the bucket structure? */

	/* Do we want to write the uniqtype directly into the heap trailer?
	 * PROBABLY, but do this LATER once we can MEASURE the BENEFIT!
	 * -- we can scrounge the union tag bits as follows:
	 *    on 32-bit x86, exploit that code is not loaded in top half of AS;
	 *    on 64-bit x86, exploit that certain bits of an addr are always 0. */
}

/* avoid dependency on libc headers */
signed long readlink(const char *restrict path, char *restrict buf,
              signed long bufsize);
char *strrchr(const char *s, int c);
void __assert_fail(const char *assertion, 
	const char *file, unsigned int line, const char *function);
void *dlopen(const char *filename, int flag);

static inline int __is_a(void *obj, const char *typename)
{
	/* We do initialization on a slow path here, rather than in a 
	 * static constructor, because adding one constructor per object
	 * file is a waste with potential for nontrivial startup time cost. */
	static _Bool tried_to_initialize = 0;
	static char libfile_name[4096];
	if (!&__uniqtypes_handle)
	{
		/* This means that we're not linked with libcrunch. 
		 * There's nothing we can do! */
		goto abort;
	}
	if (!__uniqtypes_handle)
	{
		/* This means we haven't opened the uniqtypes library. 
		 * Try that now, but don't try more than once. */
		if (!tried_to_initialize)
		{
			tried_to_initialize = 1;
			// to locate our executable, we use /proc
			int count = readlink("/proc/self/exe", libfile_name,
				sizeof libfile_name);
			if (count == -1) goto abort; // nothing we can do
			
			strncat(libfile_name, "-uniqtypes.so", sizeof libfile_name - count);
			
			__uniqtypes_handle = dlopen(libfile_name, RTLD_NOW);
			if (!__uniqtypes_handle) goto abort;
		}
		else goto abort; // already tried; give up
	}
	
	/* Now we have a working handle with which to lookup uniqued types.
	 * First we look up the allocation site.
	 * (This also yields an offset within a toplevel object.)
	 * Then we translate the allocation site to a uniqtypes rec location.
	 * (For direct calls in eagerly-loaded code, we can cache this information
	 * within uniqtypes itself. How? Make uniqtypes include a hash table with
	 * initial contents mapping allocsites to uniqtype recs. This hash table
	 * is initialized during load, but can be extended as new allocsites
	 * are discovered, e.g. indirect ones.)
	 * Then we search iteratively for a match at the offset within the toplevel
	 * object. Nonzero offsets recurse immediately, using binary search.
	 */
	
	void *object_start;
	struct trailer *heap_info = heap_index_lookup(obj, &object_start);
	if (!heap_info) goto abort;
	
	void *uniqtype = alloc_site_to_uniqtype(heap_info->alloc_site);
	//void *uniqtype = static_obj_to_uniqtype(object_start);
	//void *uniqtype = stack_frame_to_uniqtype(frame_base, file_relative_ip);
	
	assert(0);
abort:
	
}
