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

void **__libcrunch_bounds_bases_region_00;
void **__libcrunch_bounds_bases_region_2a;
void **__libcrunch_bounds_bases_region_7a;
unsigned long *__libcrunch_bounds_sizes_region_00;
unsigned long *__libcrunch_bounds_sizes_region_2a;
unsigned long *__libcrunch_bounds_sizes_region_7a;

static void *first_2a_free;
static void *first_30_free = (void*) 0x300000000000ul;

static int check_maps_cb(struct maps_entry *ent, char *linebuf, void *arg)
{
	/* Does this mapping fall within our shadowed ranges? This means
	 *     0000..0555
	 * or  2aaa..2fff
	 * or  7aaa..7fff.
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
		   is_in_range(0x000000000000ul, 0x055555555554ul, ent->first, ent->second - 1)
		|| is_in_range(0x2aaaaaaab000ul, 0x2ffffffffffful, ent->first, ent->second - 1)
		|| is_in_range(0x7aaaaaaab000ul, 0x7ffffffffffful, ent->first, ent->second - 1)
		)
	{
		// okay -- we shadow these
	}
	else if (is_in_range(0xffffffff80000000ul, 0xfffffffffffffffful, ent->first, ent->second - 1))
	{
		// okay -- it's a kernel thingy
	}
	else if (is_in_range(0x300000000000ul, 0x4ffffffffffful, ent->first, ent->second - 1))
	{
		// we assume the 3s and 4s are controlled by us -- HMM
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
	if (!__libcrunch_bounds_bases_region_00) return;
	
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
		if (is_in_range(0x2aaaaaaab000ul, 0x2ffffffffffful, (unsigned long) *p_addr,
				(unsigned long) ((char*) *p_addr + *p_length - 1)))
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

__thread unsigned long *volatile __shadow_sp;
extern __thread unsigned long *volatile __bounds_sp __attribute__((weak,alias("__shadow_sp")));
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
	 *     0000..0555
	 * or  2aaa..2fff
	 * or  7aaa..7fff.
	 * 
	 * First, CHECK this from /proc/maps at startup, and at mmap() calls.
	 */
	for_each_maps_entry(fd, get_a_line_from_maps_fd,
		linebuf, sizeof linebuf, &entry, check_maps_cb, NULL);
	if (!first_2a_free) first_2a_free = (void*) 0x2aaaaaaab000ul;
	if (!first_30_free) first_30_free = (void*) 0x300000000000ul;
	close(fd);
	
	/* HMM. Stick with XOR top-three-bits thing for the base.
	 * we need {0000,0555} -> {7000,7555}
	 *         {2aaa,2fff} -> {5aaa,5fff}
	 *         {7aaa,7fff} -> {0aaa,0fff}
	 * What about sizes? 
	 * First we want to divide the addr by 2
	 * giving  {0000,02aa} 
	 *         {1555,1aaa} 
	 *         {3d55,3fff}
	 * Then come up with a fixed offset that we can add these to
	 * so that we don't clash with any of the above (six) ranges
	 * while leaving a good chunk of free VAS in the 3s--4s.
	 * How about 0800?
	 * giving  {0800,0aaa} 
	 *         {1d55,22aa} 
	 *         {4555,47ff}.
	 * Okay, let's try it.
	 */
	__libcrunch_bounds_bases_region_00 = mmap(/* base */ (void*) 0x700000000000ul, 
		/* size */ 0x755555556000ul - 0x700000000000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_bases_region_00 != (void*) 0x700000000000ul) abort();
	__libcrunch_bounds_bases_region_2a = mmap(/* base */ (void*) 0x5aaaaaaab000ul, 
		/* size */ 0x600000000000ul - 0x5aaaaaaab000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_bases_region_2a != (void*) 0x5aaaaaaab000ul) abort();
	__libcrunch_bounds_bases_region_7a = mmap(/* base */ (void*) 0x0aaaaaaab000ul, 
		/* size */ 0x100000000000ul - 0x0aaaaaaab000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_bases_region_7a != (void*) 0x0aaaaaaab000ul) abort();
	
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
	__libcrunch_bounds_sizes_region_00 = mmap(/* base */ (void*) 0x080000000000ul, 
		/* size */ 0x0aaaaaaab000ul - 0x080000000000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|flags|MAP_NORESERVE|MAP_FIXED, fd, 0);
	if (__libcrunch_bounds_sizes_region_00 != (void*) 0x080000000000ul) abort();
	if (!libcrunch_is_loaded && fd != -1 &&
		*(unsigned *) __libcrunch_bounds_sizes_region_00 != 0xffffffffu) abort();
	__libcrunch_bounds_sizes_region_2a = mmap(/* base */ (void*) 0x1d5555556000ul, 
		/* size */ 0x22aaaaaab000ul - 0x1d5555556000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|flags|MAP_NORESERVE|MAP_FIXED, fd, 0);
	if (__libcrunch_bounds_sizes_region_2a != (void*) 0x1d5555556000ul) abort();
	__libcrunch_bounds_sizes_region_7a = mmap(/* base */ (void*) 0x455555556000ul, 
		/* size */ 0x480000000000ul - 0x455555556000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|flags|MAP_NORESERVE|MAP_FIXED, fd, 0);
	if (__libcrunch_bounds_sizes_region_7a != (void*) 0x455555556000ul) abort();

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
	Elf64_auxv_t *auxv_array_start = get_auxv((const char **) environ, environ[0]);
	if (!auxv_array_start) return;

	struct auxv_limits lims = get_auxv_limits(auxv_array_start);

	/* DON'T use __shadow_store_bounds_for here because it requires a type argument. 
	 * And we are running so early that we might not have our metadata. */
	for (const char **argvi = lims.argv_vector_start; argvi != lims.argv_vector_terminator; ++argvi)
	{
		/* We're pointing at a stored pointer. */
		*BASE_STORED(argvi) = (void*) *argvi;
		*SIZE_STORED(argvi) = strlen(*argvi) + 1;
	}
	for (const char **envi = lims.env_vector_start; envi != lims.env_vector_terminator; ++envi)
	{
		/* We're pointing at a stored pointer. */
		*BASE_STORED(envi) = (void*) *envi;
		*SIZE_STORED(envi) = strlen(*envi) + 1;
	}
	/* Leave argv's bounds there on the shadow stack for main() to pick up. */
	__push_argument_bounds_base_limit(lims.argv_vector_start, 
			(unsigned long) lims.argv_vector_start, (unsigned long) (lims.argv_vector_terminator + 1));
	
	struct link_map *exe_handle = get_exe_handle();
	void *main_addr = fake_dlsym(exe_handle, "main");
	if (!main_addr) warnx("Could not get address of main; initial cookie will be invalid");
	// FIXME: also look at static alloc records
	__push_argument_bounds_cookie(main_addr);
	
	*BASE_STORED(&environ) = lims.env_vector_start;
	*SIZE_STORED(&environ) = (lims.env_vector_terminator + 1 - lims.env_vector_start) * sizeof (char*);

	*BASE_STORED(&stdin) = stdin;
	*SIZE_STORED(&stdin) = sizeof (FILE);
	*BASE_STORED(&stdout) = stdout;
	*SIZE_STORED(&stdout) = sizeof (FILE);
	*BASE_STORED(&stderr) = stderr;
	*SIZE_STORED(&stderr) = sizeof (FILE);
	
	/* HACK: stuff below here is glibc-specific.  */
	*BASE_STORED((void**) __ctype_b_loc()) = *__ctype_b_loc();
	*SIZE_STORED((void**) __ctype_b_loc()) = 768;
	*BASE_STORED((void**) __ctype_toupper_loc()) = *__ctype_toupper_loc();
	*SIZE_STORED((void**) __ctype_toupper_loc()) = 384 * sizeof (int);
	*BASE_STORED((void**) __ctype_tolower_loc()) = *__ctype_tolower_loc();
	*SIZE_STORED((void**) __ctype_tolower_loc()) = 384 * sizeof (int);
}
