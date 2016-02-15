#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <string.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <link.h>

#include "maps.h"
#include "relf.h"
#include "systrap.h"
#include "libcrunch.h"
#include "libcrunch_private.h"

void **__libcrunch_bounds_bases_region_00;
void **__libcrunch_bounds_bases_region_2a;
void **__libcrunch_bounds_bases_region_7a;
unsigned long *__libcrunch_bounds_sizes_region_00;
unsigned long *__libcrunch_bounds_sizes_region_2a;
unsigned long *__libcrunch_bounds_sizes_region_7a;

static void *first_2a_free;
static void *first_30_free = (void*) 0x300000000000ul;
static int trap_ldso_cb(struct proc_entry *ent, char *linebuf, size_t bufsz, void *interpreter_fname_as_void)
{
	const char *interpreter_fname = (const char *) interpreter_fname_as_void;
	if (ent->x == 'x' && 0 == strcmp(interpreter_fname, ent->rest))
	{
		/* It's an executable mapping in the ld.so, so trap it. */
		trap_one_executable_region((unsigned char *) ent->first, (unsigned char *) ent->second,
			interpreter_fname, ent->w == 'w', ent->r == 'r');
	}
	
	return 0;
}

static int check_maps_cb(struct proc_entry *ent, char *linebuf, size_t bufsz, void *arg)
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
	else abort();
}
/* This is a constructor, since it's important that it happens before
 * much stuff has been memory-mapped. Unlike the main libcrunch/liballocs
 * initialisation, we don't rely on any dynamic linker or libc state. */
static void init_shadow_space(void) __attribute__((constructor,priority(102)));
extern int _etext;

void __liballocs_nudge_mmap(void **p_addr, size_t *p_length, int *p_prot, int *p_flags,
                  int *p_fd, off_t *p_offset, const void *caller)
{
	// do nothing if we're not initialized
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
	unsigned long text_segment_size = (unsigned long) &_etext; /* HACK: ABS symbol, so not relocated. */
	_Bool we_are_caller = ((char*) caller >= (char*) our_load_address 
		&& (char*) caller < (char*) our_load_address + text_segment_size);
	
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
		
		return;
	}
	
	if (we_are_caller)
	{
		/* Try to give ourselves regions *outside* the 2a range, in the 3s and 4s. */
		*p_addr = first_30_free;
		first_30_free = (char*) first_30_free + *p_length;
		return;
	}
	
	if (!first_2a_free)
	{
		/* PROBLEM: we want to put it in the 2a range, but we haven't yet walked
		 * the maps so we don't know where's free. Abort. */
		abort();
	}

	// try to give them the next free 2a. FIXME: randomize!
	if (!*p_addr)
	{
		*p_addr = first_2a_free;
		first_2a_free = (char*) first_2a_free + *p_length;
	}
	
#undef is_in_range
}

__thread unsigned long *__bounds_sp;

static void init_shadow_space(void)
{
	static char realpath_buf[4096]; /* bit of a HACK */
	/* Make sure we're trapping all syscalls within ld.so. */
	replaced_syscalls[SYS_mmap] = mmap_replacement;
	/* Get a hold of the ld.so's link map entry. How? We get it from the auxiliary
	 * vector. */
	const char *interpreter_fname = NULL;
	ElfW(auxv_t) *auxv = get_auxv((const char **) environ, &interpreter_fname);
	if (!auxv) abort();
	ElfW(auxv_t) *auxv_at_base = auxv_lookup(auxv, AT_BASE);
	if (!auxv_at_base) abort();
	const void *interpreter_base = (const void *) auxv_at_base->a_un.a_val;
	for (struct LINK_MAP_STRUCT_TAG *l = find_r_debug()->r_map; l; l = l->l_next)
	{
		if ((const void *) l->l_addr == interpreter_base)
		{
			interpreter_fname = realpath(l->l_name, &realpath_buf[0]);
		}
	}
	if (!interpreter_fname) abort();
	struct proc_entry entry;
	char proc_buf[4096];
	int ret;
	ret = snprintf(proc_buf, sizeof proc_buf, "/proc/%d/maps", getpid());
	if (!(ret > 0)) abort();
	FILE *maps = fopen(proc_buf, "r");
	if (!maps) abort();
	char linebuf[8192];
	for_each_maps_entry(fileno(maps), linebuf, sizeof linebuf, &entry, trap_ldso_cb, (void*) interpreter_fname);
	install_sigill_handler();

	#define BOUNDS_STACK_SIZE 8192
	__bounds_sp = mmap(NULL, 8192, PROT_READ|PROT_WRITE, 
		MAP_ANONYMOUS|MAP_PRIVATE|MAP_GROWSDOWN, -1, 0);

	/* Actually point bounds_sp to the highest address in the mapping. */
	__bounds_sp = (unsigned long *) ((char*) __bounds_sp + BOUNDS_STACK_SIZE - 
		sizeof (unsigned long));

	/* Map out a big chunk of virtual address space. 
	 * But one big chunk causes fragmentation. 
	 * We assume user pointers are in
	 *     0000..0555
	 * or  2aaa..2fff
	 * or  7aaa..7fff.
	 * 
	 * First, CHECK this from /proc/maps at startup, and at mmap() calls.
	 */
	rewind(maps);
	for_each_maps_entry(fileno(maps), linebuf, sizeof linebuf, &entry, check_maps_cb, NULL);
	if (!first_2a_free) first_2a_free = (void*) 0x2aaaaaaab000ul;
	if (!first_30_free) first_30_free = (void*) 0x300000000000ul;
	fclose(maps);
	
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
	
	__libcrunch_bounds_sizes_region_00 = mmap(/* base */ (void*) 0x080000000000ul, 
		/* size */ 0x0aaaaaaab000ul - 0x080000000000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_sizes_region_00 != (void*) 0x080000000000ul) abort();
	__libcrunch_bounds_sizes_region_2a = mmap(/* base */ (void*) 0x1d5555556000ul, 
		/* size */ 0x22aaaaaab000ul - 0x1d5555556000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_sizes_region_2a != (void*) 0x1d5555556000ul) abort();
	__libcrunch_bounds_sizes_region_7a = mmap(/* base */ (void*) 0x455555556000ul, 
		/* size */ 0x480000000000ul - 0x455555556000ul, PROT_READ|PROT_WRITE, 
		MAP_PRIVATE|MAP_ANONYMOUS|MAP_NORESERVE|MAP_FIXED, -1, 0);
	if (__libcrunch_bounds_sizes_region_7a != (void*) 0x455555556000ul) abort();

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
}
