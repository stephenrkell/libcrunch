/* stuff defined by libcrunch */
extern void *__uniqtypes_handle __attribute__((weak));

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
