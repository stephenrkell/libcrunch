#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <link.h>
#include <ctype.h>

#include "maps.h"
#include "relf.h"
#include "systrap.h"
#include "pageindex.h"
#include "libcrunch.h"
#include "libcrunch_private.h"
#include "libcrunch_cil_inlines.h"

/* These are defined in *both* libcrunch.c *and* stubs.c. */
extern unsigned *__libcrunch_bounds_region_00;
extern unsigned *__libcrunch_bounds_region_2a;
extern unsigned *__libcrunch_bounds_region_7f;

static void *first_2a_free;
static void *first_30_free = (void*) 0x300000000000ul;

static int check_maps_cb(struct maps_entry *ent, char *linebuf, void *arg)
{
	/* Does this mapping fall within our shadowed ranges? This means
	 *     0000..0550
	 * or  2aa0..2ff0
	 * or  7f00..7fff.
	 */
	
#define is_in_range(range_start, range_lastbyte, mapping_start, mapping_lastbyte) \
	((mapping_start) >= (range_start) && (mapping_lastbyte) <= (range_lastbyte))
	
	// we handle the 2aaa range specially since it's where we like to put mmaps
	if (is_in_range(0x2aaaaaaab000ul, 0x2ffffffffffful, ent->first, ent->second - 1))
	{
		if (!first_2a_free || (char*) ent->second > (char*) first_2a_free)
		{
			first_2a_free = (void*) ent->second;
		}
	}
	
	if (
		   is_in_range(0x000000000000ul, 0x055000000000ul, ent->first, ent->second)
		|| is_in_range(0x2a0000000000ul, 0x2f0000000000ul, ent->first, ent->second)
		|| is_in_range(0x7f0000000000ul, 0x800000000000ul, ent->first, ent->second)
		)
	{
		// okay -- we shadow these
	}
	else if (is_in_range(0xffffffff80000000ul, 0xfffffffffffffffful, ent->first, ent->second - 1))
	{
		// okay -- it's a kernel thingy
	}
	else if (is_in_range(0x300000000000ul, 0x4f0000000000ul, ent->first, ent->second))
	{
		// we assume the 3s and (most of the) 4s are controlled by us -- HMM
	}
	else abort();
}
/* This is a constructor, since it's important that it happens before
 * much stuff has been memory-mapped. Unlike the main libcrunch/liballocs
 * initialisation, we don't rely on any dynamic linker or libc state. */
static void init_shadow_space(void) __attribute__((constructor(102)));

extern void __real___liballocs_nudge_mmap(void **p_addr, size_t *p_length, int *p_prot, int *p_flags,
                  int *p_fd, off_t *p_offset, const void *caller);

void __wrap___liballocs_nudge_mmap(void **p_addr, size_t *p_length, int *p_prot, int *p_flags,
                  int *p_fd, off_t *p_offset, const void *caller)
{
	/* Call the basic nudger first. */
	__real___liballocs_nudge_mmap(p_addr, p_length, p_prot, p_flags,
                  p_fd, p_offset, caller);
	
	// do nothing more if we're not initialized
	if (!__libcrunch_bounds_region_00) return;
	
	/* We vet and perhaps tweak the mmap paramters.
	 * Currently the main use of this is in providing libcrunch's
	 * shadow space: we want to avoid creating mappings that aren't
	 * shadowable. 
	 * This means
	 * - if the caller is "us", i.e. the object containing this code, 
	 *    let it through unmolested;
	 * - if the caller is ld.so, try to make the mapping land in a good zone;
	 * - if the caller is user code, do the same. But if it's mapping something
	 *    really huge, maybe we assume they know what they're doing?
	 */
	void *our_load_address = get_highest_loaded_object_below(init_shadow_space);
	void *text_segment_end = get_local_text_segment_end();
	_Bool we_are_caller = ((char*) caller >= (char*) our_load_address 
		&& (char*) caller < (char*) text_segment_end);
	
	if (*p_addr && (*p_flags & MAP_FIXED))
	{
		/* This means the caller is really sure where they want it. 
		 * We only really trust them if it's us, but go ahead either way. 
		 * If they're in the 2a range, update our metadata. */
		if (is_in_range(0x2a0000000000ul, 0x2f0000000000ul, (unsigned long) *p_addr,
				(unsigned long) ((char*) *p_addr + *p_length)))
		{
			if (!first_2a_free || (char*) *p_addr + *p_length > (char*) first_2a_free)
			{
				first_2a_free = (char*) *p_addr + *p_length;
			}
		}
	}
	else if (/*we_are_caller && */ *p_length > BIGGEST_SANE_USER_ALLOC)
	{   // PROBLEM: "we" might be the stubs library or the preload library -- CHECK FIXME
		/* Try to give ourselves regions *outside* the 2a range, in the 3s and 4s. */
		*p_addr = first_30_free;
		first_30_free = (char*) first_30_free + *p_length;
	}
	else if (!first_2a_free)
	{
		/* PROBLEM: we want to put it in the 2a range, but we haven't yet walked
		 * the maps so we don't know where's free. Abort. */
		abort();
	}
	else if (!*p_addr)// try to give them the next free 2a. FIXME: randomize!
	{
		*p_addr = first_2a_free;
		first_2a_free = (char*) first_2a_free + *p_length;
	}
	
	// now check we did something sensible
	if (&pageindex && pageindex)
	{
		assert(pageindex[ ((uintptr_t) (*p_addr)) >> LOG_PAGE_SIZE ] == 0);
	}
	
#undef is_in_range
}

__thread unsigned long *volatile __bounds_sp;
/* HACK: volatile qualifier -- see libcrunch_cil_inlines.h and softbound-heap test case. */
/* HACK -- but actually not useful in this object. */
// _Bool __lookup_static_allocation_by_name(struct link_map *l, const char *name,
// 	void **out_addr, size_t *out_len) __attribute__((weak));

extern char **environ;

static void init_shadow_entries(void);

static void init_shadow_space(void) // constructor (declared above)
{
	#define BOUNDS_STACK_SIZE 65536
	__bounds_sp = mmap(NULL, BOUNDS_STACK_SIZE, PROT_READ|PROT_WRITE,
		MAP_ANONYMOUS|MAP_PRIVATE|MAP_GROWSDOWN, -1, 0);

	/* Actually point bounds_sp to the highest address in the mapping. */
	__bounds_sp = (unsigned long *) ((char*) __bounds_sp + BOUNDS_STACK_SIZE - 
		sizeof (unsigned long));

	struct maps_entry entry;
	char proc_buf[4096];
	int ret;
	ret = snprintf(proc_buf, sizeof proc_buf, "/proc/%d/maps", getpid());
	if (!(ret > 0)) abort();
	int fd = open(proc_buf, O_RDONLY);
	if (fd == -1) abort();
	char linebuf[8192];

	/* Map out a big chunk of virtual address space. 
	 * But one big chunk causes fragmentation. 
	 * We assume user pointers are in
	 *     0000..0550
	 * or  2aa0..2ff0
	 * or  7f00..7fff.
	 * 
	 * First, CHECK this from /proc/maps at startup, and at mmap() calls.
	 */
	for_each_maps_entry(fd, get_a_line_from_maps_fd,
		linebuf, sizeof linebuf, &entry, check_maps_cb, NULL);
	if (!first_2a_free) first_2a_free = (void*) 0x2aaaaaaab000ul;
	if (!first_30_free) first_30_free = (void*) 0x300000000000ul;
	close(fd);
	
	/* See libcrunch_cil_inlines.h for our bounds storage scheme.
	 * But in brief, we do rol 19; addb 0xa0; ror 19.
	 * The top byte of a 47-bit address is, in effect, bits 46..39.
	 * To get these into positions 7..0,
	 * i.e. positions 71..64 modulo 64,
	 * we need to rotate by decimal 25 bits, i.e. 0x19.
	 * We get  {0000,0550} -> {5000,5550}
	 *         {2a00,2f00} -> {7a00,7f00}
	 *         {7f00,7fff} -> {4f00,4fff}
	 */
	__libcrunch_bounds_region_00 = mmap(/* base */ (void*) 0x500000000000ul,
		/* size */ 0x555000000000ul - 0x500000000000ul, PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_region_00 != (void*) 0x500000000000ul) abort();
	__libcrunch_bounds_region_2a = mmap(/* base */ (void*) 0x7a0000000000ul,
		/* size */ 0x7f0000000000ul - 0x7a0000000000ul, PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_region_2a != (void*) 0x7a0000000000ul) abort();
	__libcrunch_bounds_region_7f = mmap(/* base */ (void*) 0x4f0000000000ul,
		/* size */ 0x500000000000ul - 0x4f0000000000ul, PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_region_7f != (void*) 0x4f0000000000ul) abort();
	
	/* Nasty but pleasing trick:
	 * - if the full libcrunch is not loaded, 
	 *   we try to open /dev/ones. This will give us a "size" of 0xffffffff.
	 *   This should mean that primary checks pass rather than fail.
	 */
	_Bool libcrunch_is_loaded = (&__libcrunch_really_loaded);
	int flags;
	if (libcrunch_is_loaded)
	{
		fd = -1;
		flags = MAP_ANONYMOUS;
	}
	else
	{
		fd = open("/dev/ones", O_RDONLY);
		flags = (fd == -1) ? MAP_ANONYMOUS : 0;
	}

	if (!libcrunch_is_loaded && fd != -1)
	{
		close(fd);
	}

	/* FIXME: also check we didn't/don't overlap any existing mappings. 
	 * This is difficult. A bitmap for the l0index would do it. 
	 * One bit per page's worth of l0index means each bit covers 2048 pages, 
	 * i.e. 8MB or 2^23. So to test a 2^46-byte range means 8M bits,
	 * i.e. scanning 1MB of memory which will all be page-tabled to the zero
	 * page. 
	 * 
	 * ACTUALLY it wouldn't because the l0index hasn't been init'd yet.
	 * We need to walk /proc ourselves.
	 */
	
	/* Do the init of the auxv and libc stuff. */
	init_shadow_entries();
}

static void init_shadow_entries(void)
{
	/* It's not just about wrapping functions. Initialise the globals.
	 * FIXME: not sure why SoftBound doesn't do this. */
	/* Now walk the auxv to write bounds for the argv and envp vectors. This is
	 * a bit of a HACK since we duplicate code from liballocs (which need not be 
	 * linked in right here), specifically the auxv allocator. We can't easily do
	 * this in libcrunch.c because we need to know that the shadow space has been
	 * init'd. */
	Elf64_auxv_t *auxv_array_start = get_auxv((const char **) environ, environ[0]);
	if (!auxv_array_start) return;
	
	Elf64_auxv_t *auxv_array_terminator = auxv_array_start; 
	while (auxv_array_terminator->a_type != AT_NULL) ++auxv_array_terminator;
	
	/* auxv_array_start[0] is the first word higher than envp's null terminator. */
	const char **env_vector_terminator = ((const char**) auxv_array_start) - 1;
	assert(!*env_vector_terminator);
	const char **env_vector_start = env_vector_terminator;
	while (*((char**) env_vector_start - 1)) --env_vector_start;
	
	/* argv_vector_terminator is the next word lower than envp's first entry. */
	const char **argv_vector_terminator = ((const char**) env_vector_start) - 1;
	assert(!*argv_vector_terminator);
	const char **argv_vector_start = argv_vector_terminator;
	unsigned nargs = 0;
	/* HACK: to search for the start of the array, we look for an integer that is
	 * a plausible argument count... which won't look like any pointer we're seeing. */
	#define MAX_POSSIBLE_ARGS 4194304
	while (*((uintptr_t*) argv_vector_start - 1) > MAX_POSSIBLE_ARGS)
	{
		--argv_vector_start;
		++nargs;
	}
	assert(*((uintptr_t*) argv_vector_start - 1) == nargs);
	intptr_t *p_argcount = (intptr_t*) argv_vector_start - 1;
	
	/* Now for the asciiz. We lump it all in one chunk. */
	char *asciiz_start = (char*) (auxv_array_terminator + 1);
	char *asciiz_end = asciiz_start;
	while (*(intptr_t *) asciiz_end != 0) asciiz_end += sizeof (void*);
	
	void *program_entry_point = NULL;
	ElfW(auxv_t) *found_at_entry = auxv_lookup(auxv_array_start, AT_ENTRY);
	if (found_at_entry) program_entry_point = (void*) found_at_entry->a_un.a_val;

	/* DON'T use __shadow_store_bounds_for here because it requires a type argument. 
	 * And we are running so early that we might not have our metadata. */
	for (const char **argvi = argv_vector_start; argvi != argv_vector_terminator; ++argvi)
	{
		/* We're pointing at a stored pointer. */
		*BASE_LOWBITS_STORED((void**)argvi) = (unsigned)(uintptr_t)(void*) *argvi;
		*SIZE_STORED((void**)argvi) = strlen(*argvi) + 1;
	}
	for (const char **envi = env_vector_start; envi != env_vector_terminator; ++envi)
	{
		/* We're pointing at a stored pointer. */
		*BASE_LOWBITS_STORED((void**)envi) = (unsigned)(uintptr_t)(void*) *envi;
		*SIZE_STORED((void**)envi) = strlen(*envi) + 1;
	}
	/* Leave argv's bounds there on the shadow stack for main() to pick up. */
	__push_argument_bounds_base_limit(argv_vector_start, 
			(unsigned long) argv_vector_start, (unsigned long) (argv_vector_terminator + 1));
	
	struct link_map *exe_handle = get_exe_handle();
	void *main_addr = fake_dlsym(exe_handle, "main");
	if (!main_addr) warnx("Could not get address of main; initial cookie will be invalid");
	// FIXME: also look at static alloc records
	__push_argument_bounds_cookie(main_addr);
	
	void **p = (void**) environ;
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)env_vector_start;
	*SIZE_STORED(p) = (env_vector_terminator + 1 - env_vector_start) * sizeof (char*);
	p = (void**) &stdin;
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = sizeof (FILE);
	p = (void**) &stdout;
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = sizeof (FILE);
	p = (void**) &stderr;
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = sizeof (FILE);
	
	/* HACK: stuff below here is glibc-specific.  */
	p = (void**) __ctype_b_loc();
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = 768;
	p = (void**) __ctype_toupper_loc();
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = 384 * sizeof (int);
	p = (void**) __ctype_tolower_loc();
	*BASE_LOWBITS_STORED(p) = (unsigned)(uintptr_t)*p;
	*SIZE_STORED(p) = 384 * sizeof (int);
}
