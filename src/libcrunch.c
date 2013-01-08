/* Libcrunch contains all the non-inline code that we need for doing run-time 
 * type checks on C code. */

#include "libcrunch.h"
#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

static char execfile_name[4096];
static char libfile_name[4096];

void *__uniqtypes_handle;
static _Bool tried_to_initialize;

// forward decl
static void print_exit_summary(void);

int initialize_handle(void)
{
	if (__uniqtypes_handle) return 0; // we are okay
	if (tried_to_initialize) return -1;
	tried_to_initialize = 1;
	
	// print a summary when the program exits
	atexit(print_exit_summary);

	// to locate our executable, we use /proc
	int count = readlink("/proc/self/exe", execfile_name,
		sizeof execfile_name);
	if (count == -1) return -1; // nothing we can do
	unsigned execfile_name_len = count;
	
	const char *uniqtypes_base/*_val*/ = getenv("UNIQTYPES_BASE");
	//const char *uniqtypes_base;
	//if (uniqtypes_base_val) 
	//{
	//	uniqtypes_base = strchr(uniqtypes_base_val, '=');
	//	assert(uniqtypes_base); // else we have an envvar with no '='
	//}
	//else uniqtypes_base = "";
	if (!uniqtypes_base/*_val*/) uniqtypes_base/*_val*/ = "";
	unsigned uniqtypes_base_len = strlen(uniqtypes_base/*_val*/);
	
	unsigned bytes_left = sizeof libfile_name - 1;
	libfile_name[0] = '\0';
	bytes_left--;
	// append the uniqtypes base path
	strncat(libfile_name, uniqtypes_base, bytes_left);
	bytes_left -= (bytes_left < uniqtypes_base_len) ? bytes_left : uniqtypes_base_len;
	// now append the executable name
	strncat(libfile_name, execfile_name, bytes_left);
	bytes_left -= (bytes_left < execfile_name_len) ? bytes_left : execfile_name_len;
	// now append the suffix
	strncat(libfile_name, "-uniqtypes.so", bytes_left);
	// no need to compute the last bytes_left
	// fprintf(stderr, "libcrunch: trying to open %s\n", libfile_name);

	__uniqtypes_handle = dlopen(libfile_name, RTLD_NOW);
	if (!__uniqtypes_handle)
	{
		fprintf(stderr, "dlopen() error: %s\n", dlerror());
		return -1;
	}
	
	return 0;
}

const struct rec *__libcrunch_uniqtype_void; // remember the location of the void uniqtype
/* counters */
unsigned long __libcrunch_begun;
unsigned long __libcrunch_aborted_init;
unsigned long __libcrunch_aborted_stack;
unsigned long __libcrunch_aborted_static;
unsigned long __libcrunch_aborted_unknown_storage;
unsigned long __libcrunch_aborted_unindexed_heap;
unsigned long __libcrunch_aborted_unrecognised_allocsite;
unsigned long __libcrunch_failed;
unsigned long __libcrunch_trivially_succeeded_null;
unsigned long __libcrunch_trivially_succeeded_void;
unsigned long __libcrunch_succeeded;

static void print_exit_summary(void)
{
	fprintf(stderr, "libcrunch summary: \n");
	fprintf(stderr, "checks begun:                          % 7ld\n", __libcrunch_begun);
	fprintf(stderr, "checks aborted due to init failure:    % 7ld\n", __libcrunch_aborted_init);
	fprintf(stderr, "checks aborted for stack objects:      % 7ld\n", __libcrunch_aborted_stack);
	fprintf(stderr, "checks aborted for static objects:     % 7ld\n", __libcrunch_aborted_static);
	fprintf(stderr, "checks aborted for unknown storage:    % 7ld\n", __libcrunch_aborted_unknown_storage);
	fprintf(stderr, "checks aborted for unindexed heap:     % 7ld\n", __libcrunch_aborted_unindexed_heap);
	fprintf(stderr, "checks aborted for unrecognised alloc: % 7ld\n", __libcrunch_aborted_unrecognised_allocsite);
	fprintf(stderr, "checks failed:                         % 7ld\n", __libcrunch_failed);
	fprintf(stderr, "checks trivially passed on null ptr:   % 7ld\n", __libcrunch_trivially_succeeded_null);
	fprintf(stderr, "checks trivially passed for void type: % 7ld\n", __libcrunch_trivially_succeeded_void);
	fprintf(stderr, "checks nontrivially passed:            % 7ld\n", __libcrunch_succeeded);
}

struct rec *typestr_to_uniqtype(const char *typestr)
{
	if (!typestr) return NULL;
	void *returned = dlsym(__uniqtypes_handle, typestr);
	if (!returned) return NULL;
	if (!__libcrunch_uniqtype_void && strcmp(typestr, "__uniqtype__void") == 0)
	{
		__libcrunch_uniqtype_void = (struct rec *) returned;
	}
	return (struct rec *) returned;
}

/* Force instantiation of __is_a in libcrunch, by repeating its prototype.
 * We do this so that linking an application -lcrunch is sufficient to
 * generate a weak *dynamic* reference to __is_a. */
int __is_a(const void *obj, const char *typestr);
int __is_aU(const void *obj, const struct rec *uniqtype);
int __is_a3(const void *obj, const char *typestr, const struct rec **maybe_uniqtype);

// same for initialization and, in fact, everything....
int __libcrunch_check_init(void);
enum object_memory_kind get_object_memory_kind(const void *obj);
struct rec *allocsite_to_uniqtype(const void *allocsite);
struct rec *typestr_to_uniqtype(const char *typestr);

/* Now for allocsmt.
 *
 * This is a hash- or mem-table lookup. 
 * Do we want it to be a memtable?
 * It's probably not worth it -- even for a huge application we'd have
 * at most a million allocation sites.

 * Also, what would the bucket structure be? We could thread it through
 * the allocsites entries in -uniqtypes.so. If we did this, we'd get 
 * a cluster in the low (executable) part of the AS, and another cluster
 * in a higher (shared library) region. 
 * One 4KB page of memtable would map 512 8-byte pointers into this list. 
 * Supposing that 4KB of memtable should cover at least 32KB of program
 * text, then each bucket covers 1/512 of that, i.e. 2^6 bytes, which is
 * pretty damn good. Have I calculated this correctly? Yes -- factor of 8.
 * Bump it up to a factor of 32, i.e. 256 instruction bytes. */

/* Note that we use a memtable mainly because it's easy and reduces code 
 * dependencies. In particular, it's easy to initialize using a single
 * linear scan, assuming that allocsites is sorted in address order.
 * If we wanted to initialize a hash table, the nondeterminism would
 * randomise the order and require keeping an extra data structure
 * during this scan, namely a bucket_tails[..] array. We could still
 * chain as we go along under this approach, though. */

/* Do we want to write the uniqtype directly into the heap trailer?
 * PROBABLY, but do this LATER once we can MEASURE the BENEFIT!
 * -- we can scrounge the union tag bits as follows:
 *    on 32-bit x86, exploit that code is not loaded in top half of AS;
 *    on 64-bit x86, exploit that certain bits of an addr are always 0. */

// every allocsmt entry (8 bytes) covers 256 bytes
allocsmt_entry_type *allocsmt;
 
_Bool allocsmt_initialized;
/* This is *not* a constructor. We don't want to be called too early,
 * because it might not be safe to open the -uniqtypes.so handle yet.
 * So, initialize on demand. */

void init_allocsites_memtable(void);
void init_allocsites_memtable(void)
{
	if (allocsmt_initialized) return;
	
	/* Allocate the memtable. 
	 * Assume we don't need to cover addresses >= STACK_BEGIN. */
	allocsmt = MEMTABLE_NEW_WITH_TYPE(allocsmt_entry_type, allocsmt_entry_coverage, 
		(void*) 0, (void*) STACK_BEGIN);
	assert(allocsmt != MAP_FAILED);
	
	/* Get a pointer to the start of the allocsites table. */
	struct allocsite_entry *first_entry;
	assert(__libcrunch_check_init() != -1);
	first_entry = (struct allocsite_entry *) dlsym(__uniqtypes_handle, "allocsites");
	assert(first_entry); // allocsites cannot be null anyhow

	/* We walk through allocsites, chaining together those which
	 * should be in the same bucket. */
	struct allocsite_entry *cur_ent = first_entry;
	struct allocsite_entry *prev_ent = NULL;
	unsigned current_bucket_size = 1; // out of curiosity...
	for (; cur_ent->allocsite; prev_ent = cur_ent++)
	{
		// debugging: print out entry
		/* fprintf(stderr, "allocsite entry: %p, to uniqtype at %p\n", 
			cur_ent->allocsite, cur_ent->uniqtype); */
		
		// if we've moved to a different bucket, point the table entry at us
		struct allocsite_entry **bucketpos = ALLOCSMT_FUN(ADDR, cur_ent->allocsite);
		struct allocsite_entry **prev_ent_bucketpos
		 = prev_ent ? ALLOCSMT_FUN(ADDR, prev_ent->allocsite) : NULL;
		
		// first iteration is too early to do chaining, 
		// but we do need to set up the first bucket
		if (!prev_ent || bucketpos != prev_ent_bucketpos)
		{
			// fresh bucket, so should be null
			assert(!*bucketpos);
			*bucketpos = cur_ent;
		}
		if (!prev_ent) continue;
		
		void *cur_range_base = ALLOCSMT_FUN(ADDR_RANGE_BASE, cur_ent->allocsite);
		void *prev_range_base = ALLOCSMT_FUN(ADDR_RANGE_BASE, prev_ent->allocsite);
		
		if (cur_range_base == prev_range_base)
		{
			// chain these guys together
			prev_ent->next = cur_ent;
			cur_ent->prev = prev_ent;
			
			++current_bucket_size;
		} else current_bucket_size = 1; 
		// we don't (currently) distinguish buckets of zero from buckets of one
		
		// last iteration doesn't need special handling -- next will be null,
		// prev will be set within the "if" above, if it needs to be set.
	}
	
	allocsmt_initialized = 1;
	
	// debugging: check that we can look up the first entry, if we are non-empty
	assert(!first_entry || 
		allocsite_to_uniqtype(first_entry->allocsite) == first_entry->uniqtype);
}
