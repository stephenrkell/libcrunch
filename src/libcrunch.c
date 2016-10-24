/* Libcrunch contains all the non-inline code that we need for doing run-time 
 * type checks on C code. */

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
#include <fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#ifdef USE_REAL_LIBUNWIND
#include <libunwind.h>
#endif
#include "maps.h"
#include "relf.h"
#include "libcrunch.h"
#include "libcrunch_private.h"
/* from libsystrap */
int enumerate_operands(unsigned const char *ins, unsigned const char *end,
	void *mcontext,
	void (*saw_operand)(int /*type*/, unsigned int /*bytes*/, uint32_t */*val*/,
		unsigned long */*p_reg*/, int */*p_mem_seg*/, unsigned long */*p_mem_off*/,
		int */*p_fromreg1*/, int */*p_fromreg2*/, void */*arg*/),
	void *arg
	);
void raw_exit(int) __attribute__((noreturn));
/* argh: more from libsystrap, also stolen from libdwarfpp */
enum dwarf_regs_x86_64
{
	DWARF_X86_64_RAX     = 0,
	DWARF_X86_64_RDX     = 1,
	DWARF_X86_64_RCX     = 2,
	DWARF_X86_64_RBX     = 3,
	DWARF_X86_64_RSI     = 4,
	DWARF_X86_64_RDI     = 5,
	DWARF_X86_64_RBP     = 6,
	DWARF_X86_64_RSP     = 7,
	DWARF_X86_64_R8      = 8,
	DWARF_X86_64_R9      = 9,
	DWARF_X86_64_R10     = 10,
	DWARF_X86_64_R11     = 11,
	DWARF_X86_64_R12     = 12,
	DWARF_X86_64_R13     = 13,
	DWARF_X86_64_R14     = 14,
	DWARF_X86_64_R15     = 15,
	DWARF_X86_64_RIP     = 16
};


#define NAME_FOR_UNIQTYPE(u) ((u) ? ((u)->name ?: "(unnamed type)") : "(unknown type)")

/* Heap storage sized using a "loose" data type, like void*,
 * is marked as loose, and becomes non-loose when a cast to a non-loose type.
 * Annoyingly, liballocs eagerly replaces alloc site info with uniqtype
 * info. So making queries on an object will erase its looseness.
 * FIXME: separate this out, so that non-libcrunch clients don't have to
 * explicitly preserve looseness. */
#define STORAGE_CONTRACT_IS_LOOSE(ins, site) \
(((site) != NULL) /* i.e. liballocs only just erased the alloc site */ || \
!(ins)->alloc_site_flag /* or it still hasn't */ || \
((ins)->alloc_site & 0x1ul) /* or it's marked as loose explicitly */)

int __libcrunch_debug_level;
_Bool __libcrunch_is_initialized;

FILE *crunch_stream_err;// __attribute__((visibility("hidden")));

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

static _Bool verbose;

static const char **suppression_words;
struct suppression
{
	const char *test_type_pat;
	const char *testing_function_pat;
	const char *alloc_type_pat;
};
struct suppression *suppressions;

static int print_type_cb(struct uniqtype *t, void *ignored)
{
	fprintf(crunch_stream_err, "uniqtype addr %p, name %s, size %d bytes\n", 
		t, t->name, t->pos_maxoff);
	fflush(crunch_stream_err);
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
static int do_nothing_cb(struct uniqtype *t, void *ignored)
{
	return 0; // keep going
}

void __libcrunch_scan_lazy_typenames(void *typelib_handle)
{
	/* NOTE that any object can potentially contain uniqtypes, so "typelib_handle"
	 * might not actually be the handle of a -types.so. */
	
	/* __liballocs_iterate_types is slow. use the hash table instead. */
	/* __liballocs_iterate_types(typelib_handle, match_typename_cb, NULL); */
	// HACK: while still hunting performance regressions, waste some time by doing nothing
	__liballocs_iterate_types(typelib_handle, do_nothing_cb, NULL);
	
	for (unsigned i = 0; i < lazy_heap_types_count; ++i)
	{
		if (lazy_heap_typenames[i] && !lazy_heap_types[i])
		{
			// was: look up using our hacky helper
			// const void *u = typestr_to_uniqtype_from_lib(typelib_handle, lazy_heap_typenames[i]);
					
			// build the uniqtype name and use the power of the symbol hash tables
			char buf[4096];
			char *pos = &buf[0];
			strcpy(buf, "__uniqtype__"); // use the codeless version. FIXME: what if that's not enough?
			strncat(buf, lazy_heap_typenames[i], sizeof buf - sizeof "__uniqtype__");
			buf[sizeof buf - 1] = '\0';
			// look up in global namespace
			const void *u = dlsym(RTLD_DEFAULT, buf);
			// if we found it, install it
			if (u) lazy_heap_types[i] = (struct uniqtype *) u;
		}
	}
}
int __hook_loaded_one_object_meta(struct dl_phdr_info *info, size_t size, void *data)
{
	/* NOTE: we don't do this any more.
	 *
	 * There are several reasons. One is that it's less efficient than a 
	 * compile-time solution that instruments static initializers.
	 * Another is that it complicates initialization: the hook gets called
	 * very early, when it might not be safe to run liballocs queries -- yet
	 * a query is necessary to initialize the bounds.
	 * 
	 * The static solution has the problem that bounds may not be available
	 * at compile time, in the case of something like
	 * 
	 * extern int blah[];
	 * static int *p = &blah[0];
	 *
	 * ... since the bounds for p depend on the exact size of blah, which is
	 * not decided until link time. However, that could be resolved using
	 * only link-time mechanisms, e.g. probing the size of symbol blah
	 * (if only we could insert a relocation that is expanded with the link-time
	 * size of a symbol!).
	 */
// 	struct object_metadata *p_meta = (struct object_metadata *) data;
// 	const char *real_name = dynobj_name_from_dlpi_name(info->dlpi_name, (void*) info->dlpi_addr);
// 	
// 	/* Get the bounds of the initialized data sections in this object.
// 	 * We make do with the data segment for now. */
// 	char *data_begin = NULL;
// 	char *data_end = NULL;
// 	for (int i = 0; i < info->dlpi_phnum; ++i)
// 	{
// 		if (info->dlpi_phdr[i].p_type == PT_LOAD && 
// 				(info->dlpi_phdr[i].p_flags & PF_R)
// 				&& (info->dlpi_phdr[i].p_flags & PF_W))
// 		{
// 			/* Check this is the first data phdr we've seen. */
// 			if (data_begin)
// 			{
// 				debug_printf(1, "saw multiple data segments in %s; bailing\n", real_name);
// 				return 1;
// 			}
// 			data_begin = (char*) info->dlpi_addr + info->dlpi_phdr[i].p_vaddr;
// 			data_end = data_begin + info->dlpi_phdr[i].p_memsz;
// 		}
// 	}
// 	if (!data_begin)
// 	{
// 		debug_printf(1, "found no data segment for %s; bailing\n", real_name);
// 		return 1;
// 	}
// 	
// 	/* Walk the static allocations in its meta-object, visiting any that
// 	 * - fall within the bounds of the initialized data section(s), and
// 	 * - have uniqtypes that are pointers. */
// 	if (!p_meta || !p_meta->types_handle)
// 	{
// 		debug_printf(1, "no types handle for %s; bailing\n", real_name);
// 		return 1;
// 	}
// 	
// 	void *p_statics = dlsym(p_meta->types_handle, "statics");
// 	if (!p_statics)
// 	{
// 		debug_printf(1, "no statics metadata for %s; bailing\n", real_name);
// 		return 1;
// 	}
// 	
// 	for (struct static_allocsite_entry *p_static = p_statics;
// 				p_static->entry.allocsite;
// 				++p_static)
// 	{
// 		if ((char*) p_static->entry.allocsite >= data_begin
// 					&& (char*) p_static->entry.allocsite < data_end)
// 		{
// 			/* FIXME: pointer-containing types too! */
// 			if (UNIQTYPE_IS_POINTER_TYPE(p_static->entry.uniqtype))
// 			{
// 				debug_printf(0, "saw a static pointer alloc named %s\n", p_static->name);
// 				__shadow_store_bounds_for((void**) p_static->entry.allocsite,
// 						__fetch_bounds_internal(
// 							*(void**) p_static->entry.allocsite,
// 							*(void**) p_static->entry.allocsite,
// 							UNIQTYPE_POINTEE_TYPE(p_static->entry.uniqtype)
// 						));
// 			}
// 			// else debug_printf(0, "... not of pointer type\n", p_static->name);
// 		}
// 	}
// 	
	return 0;
}

static ElfW(Dyn) *get_dynamic_entry_from_section(void *dynsec, unsigned long tag)
{
	ElfW(Dyn) *dynamic_section = dynsec;
	while (dynamic_section->d_tag != DT_NULL
		&& dynamic_section->d_tag != tag) ++dynamic_section;
	if (dynamic_section->d_tag == DT_NULL) return NULL;
	return dynamic_section;
}

static _Bool is_lazy_uniqtype(const void *u)
{
	for (unsigned i = 0; i < lazy_heap_types_count; ++i)
	{
		if (lazy_heap_types[i] == u) return 1;
	}
	return 0;
}

static _Bool prefix_pattern_matches(const char *pat, const char *str)
{
	if (!str) return 0;
	
	char *star_pos = strchr(pat, '*');
	
	return 0 == strncmp(pat, str, star_pos ? star_pos - pat : strlen(pat));
}

static _Bool test_site_matches(const char *pat /* will be saved! must not be freed */, 
		const void *test_site)
{
	_Bool result;
	Dl_info site_info = dladdr_with_cache(test_site);
	if (site_info.dli_sname)
	{
		/* okay, we can test the pat */
		result = prefix_pattern_matches(pat, site_info.dli_sname);
	}
	else
	{
		debug_printf(2, "dladdr() failed to find symbol for test site address %p\n", test_site);
		result = prefix_pattern_matches(pat, "");
	}
	return result;
}

static _Bool suppression_matches(struct suppression *s, 
		const char *test_typestr, const void *test_site, const char *alloc_typestr)
{
	/* dladdr is expensive, so 
	 * 
	 * - use it last;
	 * - cache its results (in test_site_matches. 
	 */
		
	return prefix_pattern_matches(s->test_type_pat, test_typestr)
			&& prefix_pattern_matches(s->alloc_type_pat, alloc_typestr)
			&& test_site_matches(s->testing_function_pat, test_site);
}

static _Bool is_suppressed(const char *test_typestr, const void *test_site, 
		const char *alloc_typestr)
{
	if (!suppressions) return 0;
	for (struct suppression *p = &suppressions[0];
				p->test_type_pat != NULL;
				++p)
	{
		if (suppression_matches(p, test_typestr, test_site, alloc_typestr))
		{
			++__libcrunch_failed_and_suppressed;
			return 1;
		}
	}
	return 0;
}

static _Bool done_init;
void __libcrunch_main_init(void) __attribute__((constructor(101)));
// NOTE: runs *before* the constructor in liballocs/preload.c
void __libcrunch_main_init(void)
{
	assert(!done_init);
	
	/* We check that */
	
	done_init = 1;
}

/* counters */
unsigned long __libcrunch_begun;
#ifdef LIBCRUNCH_EXTENDED_COUNTS
unsigned long __libcrunch_aborted_init;
unsigned long __libcrunch_trivially_succeeded;
unsigned long __libcrunch_checked_pointer_adjustments;
#endif
unsigned long __libcrunch_aborted_typestr;
unsigned long __libcrunch_lazy_heap_type_assignment;
unsigned long __libcrunch_failed;
unsigned long __libcrunch_failed_in_alloc;
unsigned long __libcrunch_failed_and_suppressed;
unsigned long __libcrunch_succeeded;
unsigned long __libcrunch_is_a_hit_cache;
unsigned long __libcrunch_created_invalid_pointer;
unsigned long __libcrunch_fetch_bounds_called;
unsigned long __libcrunch_fetch_bounds_missed_cache;
unsigned long __libcrunch_primary_secondary_transitions;
unsigned long __libcrunch_fault_handler_fixups;

struct __libcrunch_cache /* __thread */ __libcrunch_is_a_cache = {
	.size_plus_one = 1 + LIBCRUNCH_MAX_IS_A_CACHE_SIZE,
	.next_victim = 1
};
/* We maintain a *separate* cache of "fake bounds" that remember ranges 
 * over which we've let arithmetic go ahead because we had no type info
 * for the allocation. Without this, many programs will repeatedly report
 * failures getting bounds. We keep it separate to avoid interference with
 * the main cache. */
struct __libcrunch_cache /* __thread */ __libcrunch_fake_bounds_cache = {
	.size_plus_one = 1 + LIBCRUNCH_MAX_IS_A_CACHE_SIZE,
	.next_victim = 1
};

static unsigned long repeat_suppression_count;
// enum check
// {
// 	IS_A,
// 	LIKE_A,
// 	NAMED_A,
// 	CHECK_ARGS,
// 	IS_A_FUNCTION_REFINING
// };
//static enum check last_repeat_suppressed_check_kind;

static const void *last_failed_site;
static const struct uniqtype *last_failed_deepest_subobject_type;

struct addrlist distinct_failure_sites;

/* This filter is used to avoid repeated warnings, unless 
 * the user has requested them (verbose mode). */
static _Bool should_report_failure_at(void *site)
{
	if (verbose) return 1;
	_Bool is_unseen = !__liballocs_addrlist_contains(&distinct_failure_sites, site);
	if (is_unseen)
	{
		__liballocs_addrlist_add(&distinct_failure_sites, site);
		return 1;
	}
	else return 0;
}

static void print_exit_summary(void)
{
	if (__libcrunch_begun == 0 
		&& __libcrunch_fetch_bounds_called == 0 /* FIXME: replace with __fetch_bounds_internal failure counter */
		&& __libcrunch_created_invalid_pointer == 0
		&& !getenv("LIBCRUNCH_ALWAYS_PRINT_EXIT_SUMMARY")) return;
	
	if (repeat_suppression_count > 0)
	{
		debug_printf(0, "Suppressed %ld further occurrences of the previous error\n", 
				repeat_suppression_count);
	}
	
	fprintf(crunch_stream_err, "====================================================\n");
	fprintf(crunch_stream_err, "libcrunch summary: \n");
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
	fprintf(crunch_stream_err, "type checks begun:                         % 9ld\n", __libcrunch_begun);
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       aborted due to init failure:        % 9ld\n", __libcrunch_aborted_init);
#endif
	fprintf(crunch_stream_err, "       aborted for bad typename:           % 9ld\n", __libcrunch_aborted_typestr);
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       trivially passed:                   % 9ld\n", __libcrunch_trivially_succeeded);
#endif
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       remaining                           % 9ld\n", __libcrunch_begun - (__libcrunch_trivially_succeeded + __liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr + __libcrunch_aborted_init));
#else
	fprintf(crunch_stream_err, "       remaining                           % 9ld\n", __libcrunch_begun - (__liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr));
#endif	
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
	fprintf(crunch_stream_err, "   of which did lazy heap type assignment: % 9ld\n", __libcrunch_lazy_heap_type_assignment);
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
	fprintf(crunch_stream_err, "       failed inside allocation functions: % 9ld\n", __libcrunch_failed_in_alloc);
	fprintf(crunch_stream_err, "       failed otherwise:                   % 9ld\n", __libcrunch_failed);
	fprintf(crunch_stream_err, "                 of which user suppressed: % 9ld\n", __libcrunch_failed_and_suppressed);
	fprintf(crunch_stream_err, "       nontrivially passed:                % 9ld\n", __libcrunch_succeeded);
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
	fprintf(crunch_stream_err, "   of which hit __is_a cache:              % 9ld\n", __libcrunch_is_a_hit_cache);
	fprintf(crunch_stream_err, "----------------------------------------------------\n");
	fprintf(crunch_stream_err, "out-of-bounds pointers created:            % 9ld\n", __libcrunch_created_invalid_pointer);
	fprintf(crunch_stream_err, "accesses trapped and emulated:             % 9ld\n", 0ul /* FIXME */);
	fprintf(crunch_stream_err, "calls to __fetch_bounds:                   % 9ld\n", __libcrunch_fetch_bounds_called /* FIXME: remove */);
	fprintf(crunch_stream_err, "   of which missed cache:                  % 9ld\n", __libcrunch_fetch_bounds_missed_cache);
	fprintf(crunch_stream_err, "calls requiring secondary checks           % 9ld\n", __libcrunch_primary_secondary_transitions);
	fprintf(crunch_stream_err, "trap-pointer fixups in fault handler       % 9ld\n", __libcrunch_fault_handler_fixups);
	fprintf(crunch_stream_err, "====================================================\n");
	if (!verbose)
	{
		fprintf(crunch_stream_err, "re-run with LIBCRUNCH_VERBOSE=1 for repeat failures\n");
	}

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
		else fprintf(crunch_stream_err, "Couldn't read from smaps!\n");
	}
}

static unsigned count_separated_words(const char *str, char sep)
{
	unsigned count = 1;
	/* Count the lazy heap types */
	const char *pos = str;
	while ((pos = strchr(pos, sep)) != NULL) { ++count; ++pos; }
	return count;
}
static void fill_separated_words(const char **out, const char *str, char sep, unsigned max)
{
	unsigned n_added = 0;
	if (max == 0) return;

	const char *pos = str;
	const char *spacepos;
	do 
	{
		spacepos = strchrnul(pos, sep);
		if (spacepos - pos > 0) 
		{
			assert(n_added < max);
			out[n_added++] = __liballocs_private_strndup(pos, spacepos - pos);
		}

		pos = spacepos;
		while (*pos == sep) ++pos;
	} while (*pos != '\0' && n_added < max);
}
#define MSGLIT(s) dummy_ret = write(2, (s), sizeof (s) - 1)
#define MSGBUF(s) dummy_ret = write(2, (s), strlen((s)))
#define MSGCHAR(c) do { dummy_char = (c); dummy_ret = write(2, &dummy_char, 1); } while(0)
#define HEXCHAR(n) (((n) > 9) ? ('a' + ((n)-10)) : '0' + (n))
#define DECCHAR(n) ('0' + (n))
#define MSGADDR(a) MSGCHAR(HEXCHAR((a >> 60) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 56) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 52) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 48) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 44) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 40) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 36) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 32) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 28) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 24) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 20) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 16) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 12) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 8) % 16)); \
                   MSGCHAR(HEXCHAR((a >> 4) % 16)); \
                   MSGCHAR(HEXCHAR((a) % 16));
#define MSGSHORT(a) MSGCHAR(DECCHAR((a / 10000) % 10)); \
                  MSGCHAR(DECCHAR((a / 1000) % 10)); \
                  MSGCHAR(DECCHAR((a / 100) % 10)); \
                  MSGCHAR(DECCHAR((a / 10) % 10)); \
                  MSGCHAR(DECCHAR((a) % 10)); \

static __thread _Bool did_fixup;
void try_register_fixup(int regnum, mcontext_t *p_mcontext)
{
	const char *kindstr;
	char dummy_char;
	int dummy_ret;
	uintptr_t *p_savedval = 0;
	
	#define CASE(frag, FRAG) case DWARF_X86_64_ ##FRAG: \
		p_savedval = (uintptr_t*) &p_mcontext->gregs[REG_ ##FRAG]; break;
	switch (regnum)
	{
		CASE(r15, R15)
		CASE(r14, R14)
		CASE(r13, R13)
		CASE(r12, R12)
		CASE(rbp, RBP)
		CASE(rbx, RBX)
		CASE(r11, R11)
		CASE(r10, R10)
		CASE(r9,  R9)
		CASE(r8,  R8)
		CASE(rax, RAX)
		CASE(rcx, RCX)
		CASE(rdx, RDX)
		CASE(rsi, RSI)
		CASE(rdi, RDI)
		CASE(rip, RIP)
		case -1:
		default:
			MSGLIT("register mapping error");
			return;
	}
	#undef CASE
	
	if (!p_savedval)
	{
		MSGLIT("register lookup error");
		return;
	}
	
	MSGLIT("register ");
	MSGSHORT(regnum);
	MSGLIT(" contents 0x");
	MSGADDR(*p_savedval);
	MSGLIT(" are ");
	switch (*p_savedval >> LIBCRUNCH_TRAP_TAG_SHIFT) // FIXME: high addrs need handling
	{
		case 1: // one-past
			kindstr = "one-past";
			goto report;
		case 2: // one-prev
			kindstr = "one-before";
			goto report;
		case 3: // invalid
			kindstr = "type-invalid";
			goto report;
		default: // not one of ours
			MSGLIT("not a trap pointer");
			return;
		report:
			MSGLIT("possibly a ");
			MSGBUF(kindstr);
			const int shiftamount = 8*sizeof(uintptr_t) - LIBCRUNCH_TRAP_TAG_SHIFT;
			*p_savedval = (*p_savedval << shiftamount) >> shiftamount;
			MSGLIT(" trap pointer, so detrapping them; new value is ");
			MSGADDR(*p_savedval);
			MSGLIT("\n");
			did_fixup = 1;
			break;
	}
}

static void saw_operand_cb(int type, unsigned int bytes, uint32_t *val,
		unsigned long *p_reg, int *p_mem_seg, unsigned long *p_mem_off,
		int *p_fromreg1, int *p_fromreg2,
		void *arg)
{
	mcontext_t *p_mcontext = (mcontext_t *) arg;
	int dummy_ret;
	char dummy_char;
	
	if (type != 1 /* OP_mem */) return;
	

	/* All the operands we're interested in are memory operands.
	 * BUT we have to know how they were encoded in the instruction! */

	MSGLIT("*** memory operand was computed from ");
	if (!p_fromreg1 && !p_fromreg2)
	{
		MSGLIT("unknown values\n");
		return;
	}
	if (p_fromreg1)
	{
		if (*p_fromreg1 == -1)
		{
			MSGLIT("unknown register");
		}
		else
		{
			MSGLIT("register ");
			MSGSHORT((short) *p_fromreg1);
		}
		if (p_fromreg2) MSGLIT(" and ");
	}
	if (p_fromreg2)
	{
		if (*p_fromreg2 == -1)
		{
			MSGLIT("unknown register\n");
		}
		else
		{
			MSGLIT("register ");
			MSGSHORT((short) *p_fromreg2);
			MSGLIT("\n");
		}
	}
	
	if (p_fromreg1 && *p_fromreg1 != -1) try_register_fixup(*p_fromreg1, p_mcontext);
	if (p_fromreg2 && *p_fromreg2 != -1) try_register_fixup(*p_fromreg2, p_mcontext);
}

static void handle_sigsegv(int signum, siginfo_t *info, void *ucontext_as_void)
{
	//unsigned long *frame_base = __builtin_frame_address(0);
	//struct ibcs_sigframe *p_frame = (struct ibcs_sigframe *) (frame_base + 1);
	
	/* If we got a segfault at an address that looks like a trap pointer, then
	 * we have a few things to do.
	 * 
	 * Firstly, if it's trapped with the invalid-type tag, 
	 * we need to check the trap address against some kind of whitelist. 
	 * If the whitelist says all-clear, we
	 * -- decode the instruction to figure out where the trapped pointer was living;
	 * -- clear the trap bits at that location
	 * -- resume the program from the trapping instruction.
	 * Ideally the whitelist would be an index of all memory accesses and their
	 * uniqtypes. 
	 * If the whitelist says no, we probably want to print a warning and resume anyway,
	 * though sometimes we'll want to abort the program instead.
	 *
	 * Secondly, if it's [else] a one-past or one-prev pointer, we really
	 * can't allow this. So print a warning and (possibly) continue.
	 */
	
	// void *faulting_access_location = info->si_addr;
	/* Unbelievably, we can't seem to get the access location on Linux (si_addr
	 * is zero, and is possibly not it anyway). So scan *all* operands for
	 * trap-pointer values. This is prone to false positives! We should really
	 * check the opcode that the operand really is being used as a pointer. */
	struct ucontext *ucontext = (struct ucontext *) ucontext_as_void;
	void *faulting_code_address = (void*) ucontext->uc_mcontext.gregs[REG_RIP];
	
	int dummy_ret;
	MSGLIT("*** libcrunch caught segmentation fault\n");
	
	/* How do we make execution continue? Need to decode the instruction
	 * (to restart with a correct pointer)
	 * or emulate the access (no need to restart; but slower).
	 * We can ask libsystrap to decode the instruction operands for us. */
	did_fixup = 0;
	int ret = enumerate_operands((unsigned const char *) faulting_code_address, 
		(unsigned const char *) faulting_code_address + 16,
		&ucontext->uc_mcontext,
		saw_operand_cb,
		&ucontext->uc_mcontext);
	
	/* If we fixed something up, we can try resuming execution.
	 * Otherwise, there's no point. */
	if (did_fixup)
	{
		/* okay, resume */
		++__libcrunch_fault_handler_fixups;
		did_fixup = 0;
		return;
	}
	MSGLIT("*** libcrunch did not recover from segmentation fault, so terminating program\n");
	raw_exit(128 + SIGSEGV);
}

static void install_segv_handler(void)
{
	struct sigaction new_action = {
		.sa_sigaction = handle_sigsegv,
		.sa_flags = SA_SIGINFO
	};
	sigaction(SIGSEGV, &new_action, NULL);
}

static void early_init(void) __attribute__((constructor(101)));
static void early_init(void)
{
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
		crunch_stream_err = fopen(errvar, "w");
		if (!stream_err)
		{
			crunch_stream_err = stderr;
			debug_printf(0, "could not open %s for writing\n", errvar);
		}
	} else crunch_stream_err = stderr;
	assert(crunch_stream_err);

	const char *debug_level_str = getenv("LIBCRUNCH_DEBUG_LEVEL");
	if (debug_level_str) __libcrunch_debug_level = atoi(debug_level_str);
	
	verbose = __libcrunch_debug_level >= 1 || getenv("LIBCRUNCH_VERBOSE");
	
	// print a summary when the program exits
	atexit(print_exit_summary);
}

static void clear_mem_refbits(void)
{
	int fd = open("/proc/self/clear_refs", O_WRONLY);
	if (fd == -1) abort();
	int dummy_ret __attribute__((unused)) = write(fd, "1\n", sizeof "1\n" - 1);
	close(fd);
}

int __libcrunch_global_init(void)
{
	if (__libcrunch_is_initialized) return 0; // we are okay

	// don't try more than once to initialize
	static _Bool tried_to_initialize;
	if (tried_to_initialize) return -1;
	tried_to_initialize = 1;
	
	// we must have initialized liballocs
	__liballocs_ensure_init();
	
	/* We always include "signed char" in the lazy heap types. (FIXME: this is a 
	 * C-specificity we'd rather not have here, but live with it for now.
	 * Perhaps the best way is to have "uninterpreted_sbyte" and make signed_char
	 * an alias for it.) We count the other ones. */
	const char *lazy_heap_types_str = getenv("LIBCRUNCH_LAZY_HEAP_TYPES");
	lazy_heap_types_count = 1;
	unsigned upper_bound = 2; // signed char plus one string with zero spaces
	if (lazy_heap_types_str)
	{
		unsigned count = count_separated_words(lazy_heap_types_str, ' ');
		upper_bound += count;
		lazy_heap_types_count += count;
	}
	/* Allocate and populate. */
	lazy_heap_typenames = calloc(upper_bound, sizeof (const char *));
	lazy_heap_types = calloc(upper_bound, sizeof (struct uniqtype *));

	// the first entry is always signed char
	lazy_heap_typenames[0] = "signed_char$8";
	if (lazy_heap_types_str)
	{
		fill_separated_words(&lazy_heap_typenames[1], lazy_heap_types_str, ' ',
				upper_bound - 1);
	}
	
// 	/* We have to scan for lazy heap types *in link order*, so that we see
// 	 * the first linked definition of any type that is multiply-defined.
// 	 * Do a scan now; we also scan when loading a types object, and when loading
// 	 * a user-dlopen()'d object. 
// 	 * 
// 	 * We don't use dl_iterate_phdr because it doesn't give us the link_map * itself. 
// 	 * Instead, walk the link map directly, like a debugger would
// 	 *                                           (like I always knew somebody should). */
// 	// grab the executable's end address
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
	
	/* Load the suppression list from LIBCRUNCH_SUPPRESS. It's a space-separated
	 * list of triples <test-type-pat, testing-function-pat, alloc-type-pat>
	 * where patterns can end in "*" to indicate prefixing. */
	unsigned suppressions_count = 0;
	const char *suppressions_str = getenv("LIBCRUNCH_SUPPRESSIONS");
	if (suppressions_str)
	{
		unsigned suppressions_count = count_separated_words(suppressions_str, ' ');
		suppression_words = calloc(1 + suppressions_count, sizeof (char *));
		assert(suppression_words);
		suppressions = calloc(1 + suppressions_count, sizeof (struct suppression));
		assert(suppressions);
		
		fill_separated_words(&suppression_words[0], suppressions_str, ' ', suppressions_count);
		
		for (const char **p_word = &suppression_words[0];
				*p_word;
				++p_word)
		{
			unsigned n_comma_sep = count_separated_words(*p_word, ',');
			if (n_comma_sep != 3)
			{
				debug_printf(1, "invalid suppression: %s\n", *p_word);
			}
			else
			{
				fill_separated_words(
					&suppressions[p_word - &suppression_words[0]].test_type_pat,
					*p_word,
					',',
					3);
			}
		}
	}

	// we need a segv handler to handle uses of trapped pointers
	install_segv_handler();
	
	// for sane memory usage measurement, consider referencedness to start now
	clear_mem_refbits();
	
	__libcrunch_is_initialized = 1;

	debug_printf(1, "libcrunch successfully initialized\n");
	
	return 0;
}

static void check_cache_sanity(struct __libcrunch_cache *cache)
{
	__libcrunch_check_cache_sanity(cache);
}

static void cache_is_a(const void *obj_base, const void *obj_limit, const struct uniqtype *t, 
	_Bool result, unsigned short period, const void *alloc_base)
{
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
#ifdef LIBCRUNCH_CACHE_REPLACE_FIFO
	unsigned pos = __libcrunch_is_a_cache.next_victim;
#else
	// "one plus the index of the least significant 0-bit" of validity
	unsigned pos = __builtin_ffs(~(__libcrunch_is_a_cache.validity));
	assert(pos <= __libcrunch_is_a_cache.size_plus_one);
	if (pos == __libcrunch_is_a_cache.size_plus_one)
	{
		pos = __libcrunch_is_a_cache.tail_mru;
		assert(pos != 0);
	}
#endif
	// unsigned pos = __libcrunch_is_a_cache.next_victim;
	__libcrunch_is_a_cache.entries[pos] = (struct __libcrunch_cache_entry_s) {
		.obj_base = obj_base,
		.obj_limit = obj_limit,
		.uniqtype = (void*) t,
		.period = period,
		.result = result,
		.prev_mru = __libcrunch_is_a_cache.entries[pos].prev_mru,
		.next_mru = __libcrunch_is_a_cache.entries[pos].next_mru
	};
	// bump us to the top
	__libcrunch_cache_bump_mru(&__libcrunch_is_a_cache, pos);
	__libcrunch_cache_bump_victim(&__libcrunch_is_a_cache, pos);
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
}

static void cache_bounds(const void *obj_base, const void *obj_limit, const struct uniqtype *t, 
	_Bool result, unsigned short period, const void *alloc_base)
{
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
#ifdef LIBCRUNCH_CACHE_REPLACE_FIFO
	unsigned pos = __libcrunch_is_a_cache.next_victim;
#else
	unsigned pos = __builtin_ffs(~(__libcrunch_is_a_cache.validity));
	assert(pos <= __libcrunch_is_a_cache.size_plus_one);
	if (pos == __libcrunch_is_a_cache.size_plus_one)
	{
		pos = __libcrunch_is_a_cache.tail_mru;
		assert(pos != 0);
	}
#endif
	__libcrunch_is_a_cache.entries[pos] = (struct __libcrunch_cache_entry_s) {
		.obj_base = obj_base,
		.obj_limit = obj_limit,
		.uniqtype = (void*) t,
		.period = period,
		.result = 1 /* FIXME */,
		/* don't touch the list -- cache_bump will do it. */
		.prev_mru = __libcrunch_is_a_cache.entries[pos].prev_mru,
		.next_mru = __libcrunch_is_a_cache.entries[pos].next_mru
	};
	// bump us to the top (inserting us if we're invalid)
	__libcrunch_cache_bump_mru(&__libcrunch_is_a_cache, pos);
	__libcrunch_cache_bump_victim(&__libcrunch_is_a_cache, pos);
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
}

static void cache_fake_bounds(const void *obj_base, const void *obj_limit, const struct uniqtype *t, 
	_Bool result, unsigned short period, const void *alloc_base)
{
	assert((check_cache_sanity(&__libcrunch_fake_bounds_cache), 1));
#ifdef LIBCRUNCH_CACHE_REPLACE_FIFO
	unsigned pos = __libcrunch_fake_bounds_cache.next_victim;
#else
	unsigned pos = __builtin_ffs(~(__libcrunch_fake_bounds_cache.validity));
	assert(pos <= __libcrunch_fake_bounds_cache.size_plus_one);
	if (pos == __libcrunch_fake_bounds_cache.size_plus_one)
	{
		pos = __libcrunch_fake_bounds_cache.tail_mru;
		assert(pos != 0);
	}
#endif
	/* Create the new entry and put it at the head. */
	__libcrunch_fake_bounds_cache.entries[pos] = (struct __libcrunch_cache_entry_s) {
		.obj_base = obj_base,
		.obj_limit = obj_limit,
		.uniqtype = (void*) t,
		.period = period,
		.result = 1 /* FIXME */,
		/* don't touch the list -- cache_bump will do it */
		.prev_mru = __libcrunch_fake_bounds_cache.entries[pos].prev_mru,
		.next_mru = __libcrunch_fake_bounds_cache.entries[pos].next_mru
	};
	// bump us to the top
	__libcrunch_cache_bump_mru(&__libcrunch_fake_bounds_cache, pos);
	__libcrunch_cache_bump_victim(&__libcrunch_fake_bounds_cache, pos);
	assert((check_cache_sanity(&__libcrunch_fake_bounds_cache), 1));
}
/* FIXME: rewrite these */
void __libcrunch_uncache_all(const void *allocptr, size_t size)
{
	__libcrunch_uncache_is_a(allocptr, size);
}
void __libcrunch_uncache_is_a(const void *allocptr, size_t size)
{
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
	for (unsigned i = 1; i < __libcrunch_is_a_cache.size_plus_one; ++i)
	{
		if (__libcrunch_is_a_cache.validity & (1u << (i-1)))
		{
			assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
			/* Uncache any object beginning anywhere within the passed-in range. */
			if ((char*) __libcrunch_is_a_cache.entries[i].obj_base >= (char*) allocptr
					 && (char*) __libcrunch_is_a_cache.entries[i].obj_base < (char*) allocptr + size)
			{
				// unset validity and make this the next victim
				__libcrunch_cache_unlink(&__libcrunch_is_a_cache, i);
				__libcrunch_is_a_cache.next_victim = i;
			}
			assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
		}
	}
	assert((check_cache_sanity(&__libcrunch_is_a_cache), 1));
}

int __is_a_internal(const void *obj, const void *arg)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	struct allocator *a = NULL;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype,
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0)) return 1; // liballocs has already counted this abort

	signed target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;
	void *range_base;
	void *range_limit;
	unsigned short period = (alloc_uniqtype->pos_maxoff > 0) ? alloc_uniqtype->pos_maxoff : 0;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 65535 /* HACK test for -1 */
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		// HACK: for now, assume that the repetition continues to the end
		range_limit = (char*) alloc_start + alloc_size_bytes;
		target_offset_within_uniqtype %= alloc_uniqtype->pos_maxoff;
		range_base = (char*) alloc_start + target_offset_within_uniqtype; // FIXME: please check
	}
	else
	{
		/* HMM. Is this right? What about regularity *not* at top-level? */
		range_limit = (char*) obj + (test_uniqtype ? test_uniqtype->pos_maxoff : 1);
		range_base = (char*) obj;
	}
	
	/* FIXME: static allocations are cacheable *only* so long as we 
	 * purge the cache on dynamic unloading (which we currently don't).
	 * FIXME: we need to distinguish alloca'd from stack'd allocations, somehow. */
	_Bool is_cacheable = a->is_cacheable /*|| ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)*/;
	
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;

	signed cumulative_offset_searched = 0;
	_Bool success = __liballocs_find_matching_subobject(target_offset_within_uniqtype, 
			cur_obj_uniqtype, (struct uniqtype *) test_uniqtype, &cur_obj_uniqtype, 
			&target_offset_within_uniqtype, &cumulative_offset_searched);
	
	if (__builtin_expect(success, 1))
	{
		/* populate cache */
		if (is_cacheable) cache_is_a(range_base, range_limit, test_uniqtype, 1,
			period, alloc_start);

		++__libcrunch_succeeded;
		return 1;
	}
	
	// if we got here, we might still need to apply lazy heap typing
	if (__builtin_expect(a == &__generic_malloc_allocator /* FIXME */
			&& is_lazy_uniqtype(alloc_uniqtype)
			&& !__currently_allocating, 0))
	{
		struct insert *ins = __liballocs_get_insert(obj);
		assert(ins);
		if (STORAGE_CONTRACT_IS_LOOSE(ins, alloc_site))
		{
			++__libcrunch_lazy_heap_type_assignment;
			
			// update the heap chunk's info to say that its type is (strictly) our test_uniqtype
			ins->alloc_site_flag = 1;
			ins->alloc_site = (uintptr_t) test_uniqtype;
			if (is_cacheable) cache_is_a(range_base, range_limit, test_uniqtype, 1, period, alloc_start);
		
			return 1;
		}
	}
	
	// if we got here, the check failed
	if (is_cacheable) cache_is_a(range_base, range_limit, test_uniqtype, 0, period, alloc_start);
	if (__currently_allocating || __currently_freeing)
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		
		if (!is_suppressed(test_uniqtype->name, __builtin_return_address(0), alloc_uniqtype ? alloc_uniqtype->name : NULL))
		{
			if (should_report_failure_at(__builtin_return_address(0)))
			{
				if (last_failed_site == __builtin_return_address(0)
						&& last_failed_deepest_subobject_type == cur_obj_uniqtype)
				{
					++repeat_suppression_count;
				}
				else
				{
					if (repeat_suppression_count > 0)
					{
						debug_printf(0, "Suppressed %ld further occurrences of the previous error\n", 
								repeat_suppression_count);
					}

					debug_printf(0, "Failed check __is_a(%p, %p a.k.a. \"%s\") at %p (%s); "
							"obj is %ld bytes into a %s%s%s "
							"(deepest subobject: %s at offset %d) "
							"originating at %p\n", 
						obj, test_uniqtype, test_uniqtype->name,
						__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)), 
						(long)((char*) obj - (char*) alloc_start),
						a ? a->name : "(no allocator)",
						(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
						NAME_FOR_UNIQTYPE(alloc_uniqtype), 
						(cur_obj_uniqtype ? 
							((cur_obj_uniqtype == alloc_uniqtype) ? "(the same)" : cur_obj_uniqtype->name) 
							: "(none)"), 
						cumulative_offset_searched, 
						alloc_site);

					last_failed_site = __builtin_return_address(0);
					last_failed_deepest_subobject_type = cur_obj_uniqtype;

					repeat_suppression_count = 0;
				}
			}
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
	struct allocator *a;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	void *caller_address = __builtin_return_address(0);
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype, 
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0)) return 1; // liballocs has already counted this abort
	
	signed target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 0 
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		target_offset_within_uniqtype %= alloc_uniqtype->pos_maxoff;
	}
	
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
					== pointer_to___uniqtype__signed_char
			|| test_uniqtype->contained[i_test_subobj].ptr->contained[0].ptr
					== pointer_to___uniqtype__unsigned_char), 0))
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
		if (!is_suppressed(test_uniqtype->name, __builtin_return_address(0), alloc_uniqtype ? alloc_uniqtype->name : NULL))
		{
			if (should_report_failure_at(__builtin_return_address(0)))
			{
				debug_printf(0, "Failed check __like_a(%p, %p a.k.a. \"%s\") at %p (%s), allocation was a %s%s%s originating at %p\n", 
					obj, test_uniqtype, test_uniqtype->name,
					__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)),
					a ? a->name : "(no allocator)",
					(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
					NAME_FOR_UNIQTYPE(alloc_uniqtype), 
					alloc_site);
			}
		}
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
	struct allocator *a;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	void *caller_address = __builtin_return_address(0);
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype, 
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0)) return 1; // we've already counted it
	
	signed target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 0 
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		target_offset_within_uniqtype %= alloc_uniqtype->pos_maxoff;
	}
	
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
		if (!is_suppressed(test_typestr, __builtin_return_address(0), alloc_uniqtype ? alloc_uniqtype->name : NULL))
		{
			if (should_report_failure_at(__builtin_return_address(0)))
			{
				debug_printf(0, "Failed check __named_a(%p, \"%s\") at %p (%s), allocation was a %s%s%s originating at %p\n", 
					obj, test_typestr,
					__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)),
					a ? a->name : "(no allocator)",
					(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
					NAME_FOR_UNIQTYPE(alloc_uniqtype),
					alloc_site);
			}
		}
	}
	return 1; // HACK: so that the program will continue
}

int 
__check_args_internal(const void *obj, int nargs, ...)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();

	struct allocator *a;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;

	struct uniqtype *fun_uniqtype = NULL;
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&fun_uniqtype,
		&alloc_site);
	
	if (err != NULL) return 1;
	
	assert(fun_uniqtype);
	assert(alloc_start == obj);
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
		if (UNIQTYPE_IS_POINTER_TYPE(expected_arg))
		{
			struct uniqtype *expected_arg_pointee_type = UNIQTYPE_POINTEE_TYPE(expected_arg);
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
	
	/* NOTE that __check_args is not just one "test"; it's many. 
	 * So we don't maintain separate counts here; our use of __is_aU above
	 * will create many counts. */
	
	return success ? 0 : i; // 0 means success here
}

int __is_a_function_refining_internal(const void *obj, const void *arg)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	struct allocator *a = NULL;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	signed target_offset_within_uniqtype;
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype,
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0))
	{
		return 1;
	}
	
	/* If we're offset-zero, that's good... */
	if (alloc_start == obj)
	{
		/* If we're an exact match, that's good.... */
		if (alloc_uniqtype == arg)
		{
			++__libcrunch_succeeded;
			return 1;
		}
		else
		{
			/* If we're not a function, that's bad. */
			if (UNIQTYPE_IS_SUBPROGRAM(alloc_uniqtype))
			{
				/* If our argument counts don't match, that's bad. */
				if (alloc_uniqtype->array_len == test_uniqtype->array_len)
				{
					/* For each argument, we want to make sure that 
					 * the "implicit" cast done on the argument, from
					 * the cast-from type to the cast-to type, i.e. that 
					 * the passed argument *is_a* received argument, i.e. that
					 * the cast-to argument *is_a* cast-from argument. */
					_Bool success = 1;
					/* Recall: return type is in [0] and arguments are in 1..array_len. */
					
					/* Would the cast from the return value to the post-cast return value
					 * always succeed? If so, this cast is okay. */
					struct uniqtype *alloc_return_type = alloc_uniqtype->contained[0].ptr;
					struct uniqtype *cast_return_type = test_uniqtype->contained[0].ptr;
					
					/* HACK: a little bit of C-specifity is creeping in here.
					 * FIXME: adjust this to reflect sloppy generic-pointer-pointer matches! 
					      (only if LIBCRUNCH_STRICT_GENERIC_POINTERS not set) */
					/* HACK: referencing uniqtypes directly from libcrunch is problematic
					 * for COMDAT / section group / uniqing reasons. Ideally we wouldn't
					 * do this. To prevent non-uniquing, we need to avoid linking
					 * uniqtypes into the preload .so. But we can't rely on any particular
					 * uniqtypes being in the executable; and if they're in a library
					 * won't let us bind to them from the preload library (whether the
					 * library is linked at startup or later, as it happens).  One workaround:
					 * use the _nonshared.a hack for -lcrunch_stubs too, so that all 
					 * libcrunch-enabled executables necessarily have __uniqtype__void
					 * and __uniqtype__signed_char in the executable.
					 * ARGH, but we still can't bind to these from the preload lib.
					 * (That's slightly surprising semantics, but it's what I observe.)
					 * We have to use dynamic linking. */
					#define would_always_succeed(from, to) \
						( \
							!UNIQTYPE_IS_POINTER_TYPE((to)) \
						||  (UNIQTYPE_POINTEE_TYPE((to)) == pointer_to___uniqtype__void) \
						||  (UNIQTYPE_POINTEE_TYPE((to)) == pointer_to___uniqtype__signed_char) \
						||  (UNIQTYPE_IS_POINTER_TYPE((from)) && \
							__liballocs_find_matching_subobject( \
							/* target_offset_within_uniqtype */ 0, \
							/* cur_obj_uniqtype */ UNIQTYPE_POINTEE_TYPE((from)), \
							/* test_uniqtype */ UNIQTYPE_POINTEE_TYPE((to)), \
							/* last_attempted_uniqtype */ NULL, \
							/* last_uniqtype_offset */ NULL, \
							/* p_cumulative_offset_searched */ NULL)) \
						)
						
					/* ARGH. Are these the right way round?  
					 * The "implicit cast" is from the alloc'd return type to the 
					 * cast-to return type. */
					success &= would_always_succeed(alloc_return_type, cast_return_type);
					
					if (success) for (int i = 1; i <= alloc_uniqtype->array_len; ++i)
					{
						/* ARGH. Are these the right way round?  
						 * The "implicit cast" is from the cast-to arg type to the 
						 * alloc'd arg type. */
						success &= would_always_succeed(
							test_uniqtype->contained[i].ptr,
							alloc_uniqtype->contained[i].ptr
						);

						if (!success) break;
					}
					
					if (success)
					{
						++__libcrunch_succeeded;
						return 1;
					}
				}
			}
		}
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
		if (!is_suppressed(test_uniqtype->name, __builtin_return_address(0), alloc_uniqtype ? alloc_uniqtype->name : NULL))
		{
			if (should_report_failure_at(__builtin_return_address(0)))
			{
				if (last_failed_site == __builtin_return_address(0)
						&& last_failed_deepest_subobject_type == alloc_uniqtype)
				{
					++repeat_suppression_count;
				}
				else
				{
					if (repeat_suppression_count > 0)
					{
						debug_printf(0, "Suppressed %ld further occurrences of the previous error\n", 
								repeat_suppression_count);
					}

					debug_printf(0, "Failed check __is_a_function_refining(%p, %p a.k.a. \"%s\") at %p (%s), "
							"found an allocation of a %s%s%s "
							"originating at %p\n", 
						obj, test_uniqtype, test_uniqtype->name,
						__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)), 
						a ? a->name : "(no allocator)",
						(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
						NAME_FOR_UNIQTYPE(alloc_uniqtype),
						alloc_site);
					last_failed_site = __builtin_return_address(0);
					last_failed_deepest_subobject_type = alloc_uniqtype;
					repeat_suppression_count = 0;
				}
			}
		}
	}
	return 1; // HACK: so that the program will continue
}

/* This helper is short-circuiting: it doesn't tell you the precise degree 
 * of the pointer, only whether it's at least d. */
static _Bool pointer_has_degree(struct uniqtype *t, int d)
{
	while (d > 0)
	{
		if (!UNIQTYPE_IS_POINTER_TYPE(t)) return 0;
		t = UNIQTYPE_POINTEE_TYPE(t);
		assert(t);
		--d;
	}
	return 1;
}

static _Bool pointer_degree_and_ultimate_pointee_type(struct uniqtype *t, int *out_d, 
		struct uniqtype **out_ultimate_pointee_type)
{
	int d = 0;
	while (UNIQTYPE_IS_POINTER_TYPE(t))
	{
		++d;
		t = UNIQTYPE_POINTEE_TYPE(t);
	}
	*out_d = d;
	*out_ultimate_pointee_type = t;
	return 1;
}

static _Bool is_generic_ultimate_pointee(struct uniqtype *ultimate_pointee_type)
{
	return ultimate_pointee_type == pointer_to___uniqtype__void 
		|| ultimate_pointee_type == pointer_to___uniqtype__signed_char
		|| ultimate_pointee_type == pointer_to___uniqtype__unsigned_char;
}

static _Bool holds_pointer_of_degree(struct uniqtype *cur_obj_uniqtype, int d, signed target_offset)
{
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;
	signed target_offset_within_uniqtype = target_offset;

	/* Descend the subobject hierarchy until we can't go any further (since pointers
	 * are atomic. */
	_Bool success = 1;
	while (success)
	{
		success = __liballocs_first_subobject_spanning(
			&target_offset_within_uniqtype, &cur_obj_uniqtype, &cur_containing_uniqtype,
			&cur_contained_pos);
	}

	if (target_offset_within_uniqtype == 0 && UNIQTYPE_IS_POINTER_TYPE(cur_obj_uniqtype))
	{
		_Bool depth_okay = pointer_has_degree(cur_obj_uniqtype, d);
		if (depth_okay)
		{
			return 1;
		} else return 0;
	}
	
	return 0;
}

static int pointer_degree(struct uniqtype *t)
{
	_Bool success;
	int d;
	struct uniqtype *dontcare;
	success = pointer_degree_and_ultimate_pointee_type(t, &d, &dontcare);
	return success ? d : -1;
}

static int is_generic_pointer_type_of_degree_at_least(struct uniqtype *t, int must_have_d)
{
	_Bool success;
	int d;
	struct uniqtype *ultimate;
	success = pointer_degree_and_ultimate_pointee_type(t, &d, &ultimate);
	if (success && d >= must_have_d && is_generic_ultimate_pointee(ultimate)) return d;
	else return 0;
}

static _Bool is_generic_pointer_type(struct uniqtype *t)
{
	return is_generic_pointer_type_of_degree_at_least(t, 1);
}

static void
reinstate_looseness_if_necessary(
    const void *alloc_start, const void *alloc_site,
    struct uniqtype *alloc_uniqtype
)
{
	/* Unlike other checks, we want to preserve looseness of the target block's 
	 * type, if it's a pointer type. So set the loose flag if necessary. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) 
			&& alloc_site != NULL
			&& UNIQTYPE_IS_POINTER_TYPE(alloc_uniqtype))
	{
		struct insert *ins = __liballocs_get_insert(alloc_start);
		//	(void*) alloc_start, malloc_usable_size((void*) alloc_start)
		//);
		if (ins->alloc_site_flag)
		{
			assert(0 == ins->alloc_site & 0x1ul);
			ins->alloc_site |= 0x1ul;
		}
	}
}

int __loosely_like_a_internal(const void *obj, const void *arg)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	struct uniqtype *test_uniqtype = (struct uniqtype *) arg;
	struct allocator *a;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	void *caller_address = __builtin_return_address(0);
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype, 
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0)) return 1; // liballocs has already counted this abort
	
	/* HACK */
	reinstate_looseness_if_necessary(alloc_start, alloc_site, alloc_uniqtype);
	
	signed target_offset_within_alloc_uniqtype = (char*) obj - (char*) alloc_start;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 0 
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		target_offset_within_alloc_uniqtype %= alloc_uniqtype->pos_maxoff;
	}
	
	struct uniqtype *cur_alloc_subobj_uniqtype = alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;
	
	/* Descend the subobject hierarchy until our target offset is zero, i.e. we 
	 * find the outermost thing in the subobject tree that starts at the address
	 * we were passed (obj). */
	while (target_offset_within_alloc_uniqtype != 0)
	{
		_Bool success = __liballocs_first_subobject_spanning(
				&target_offset_within_alloc_uniqtype, &cur_alloc_subobj_uniqtype, &cur_containing_uniqtype,
				&cur_contained_pos);
		if (!success) goto loosely_like_a_failed;
	}
	
	do
	{
		
	// trivially, identical types are like one another
	if (test_uniqtype == cur_alloc_subobj_uniqtype) goto loosely_like_a_succeeded;
	
	// if our check type is a pointer type
	int real_degree;
	if (UNIQTYPE_IS_POINTER_TYPE(test_uniqtype)
			&& 0 != (real_degree = is_generic_pointer_type_of_degree_at_least(test_uniqtype, 1)))
	{
		// the pointed-to object must have at least the same degree
		if (holds_pointer_of_degree(cur_alloc_subobj_uniqtype, real_degree, 0))
		{
			++__libcrunch_succeeded;
			return 1;
		}
		
		// nothing is like a non-generic pointer
		goto try_deeper;
	}
	
	// arrays are special
	_Bool matches;
	if (__builtin_expect((cur_alloc_subobj_uniqtype->is_array || test_uniqtype->is_array), 0))
	{
		matches = 
			test_uniqtype == cur_alloc_subobj_uniqtype
		||  (test_uniqtype->is_array && test_uniqtype->array_len == 1 
				&& test_uniqtype->contained[0].ptr == cur_alloc_subobj_uniqtype)
		||  (cur_alloc_subobj_uniqtype->is_array && cur_alloc_subobj_uniqtype->array_len == 1
				&& cur_alloc_subobj_uniqtype->contained[0].ptr == test_uniqtype);
		/* We don't need to allow an array of one blah to be like a different
		 * array of one blah, because they should be the same type. 
		 * FIXME: there's a difficult case: an array of statically unknown length, 
		 * which happens to have length 1. */
		if (matches) goto loosely_like_a_succeeded; else goto try_deeper;
	}
	
	/* If we're not an array and nmemb is zero, we might have base types with
	 * signedness complements. */
	if (__builtin_expect(
			!cur_alloc_subobj_uniqtype->is_array && !test_uniqtype->is_array
			&&  cur_alloc_subobj_uniqtype->nmemb == 0 && test_uniqtype->nmemb == 0, 0))
	{
		/* Does the cur obj type have a signedness complement matching the test type? */
		if (cur_alloc_subobj_uniqtype->contained[0].ptr == test_uniqtype) goto loosely_like_a_succeeded;
		/* Does the test type have a signedness complement matching the cur obj type? */
		if (test_uniqtype->contained[0].ptr == cur_alloc_subobj_uniqtype) goto loosely_like_a_succeeded;
	}
	
	/* Okay, we can start the like-a test: for each element in the test type, 
	 * do we have a type-equivalent in the object type?
	 * 
	 * We make an exception for arrays of char (signed or unsigned): if an
	 * element in the test type is such an array, we skip over any number of
	 * fields in the object type, until we reach the offset of the end element.  */
	unsigned i_obj_subobj = 0, i_test_subobj = 0;
	if (test_uniqtype != cur_alloc_subobj_uniqtype) debug_printf(0, "__loosely_like_a proceeding on subobjects of (test) %s and (object) %s\n",
		NAME_FOR_UNIQTYPE(test_uniqtype), NAME_FOR_UNIQTYPE(cur_alloc_subobj_uniqtype));
	for (; 
		i_obj_subobj < cur_alloc_subobj_uniqtype->nmemb && i_test_subobj < test_uniqtype->nmemb; 
		++i_test_subobj, ++i_obj_subobj)
	{
		debug_printf(0, "Subobject types are (test) %s and (object) %s\n",
			NAME_FOR_UNIQTYPE((struct uniqtype *) test_uniqtype->contained[i_test_subobj].ptr), 
			NAME_FOR_UNIQTYPE((struct uniqtype *) cur_alloc_subobj_uniqtype->contained[i_obj_subobj].ptr));
		
		if (__builtin_expect(test_uniqtype->contained[i_test_subobj].ptr->is_array
			&& (test_uniqtype->contained[i_test_subobj].ptr->contained[0].ptr
					== pointer_to___uniqtype__signed_char
			|| test_uniqtype->contained[i_test_subobj].ptr->contained[0].ptr
					== pointer_to___uniqtype__unsigned_char), 0))
		{
			// we will skip this field in the test type
			signed target_off =
				test_uniqtype->nmemb > i_test_subobj + 1
			 ?  test_uniqtype->contained[i_test_subobj + 1].offset
			 :  test_uniqtype->contained[i_test_subobj].offset
			      + test_uniqtype->contained[i_test_subobj].ptr->pos_maxoff;
			
			// ... if there's more in the test type, advance i_obj_subobj
			while (i_obj_subobj + 1 < cur_alloc_subobj_uniqtype->nmemb &&
				cur_alloc_subobj_uniqtype->contained[i_obj_subobj + 1].offset < target_off) ++i_obj_subobj;
			/* We fail if we ran out of stuff in the actual object type
			 * AND there is more to go in the test (cast-to) type. */
			if (i_obj_subobj + 1 >= cur_alloc_subobj_uniqtype->nmemb
			 && test_uniqtype->nmemb > i_test_subobj + 1) goto try_deeper;
				
			continue;
		}
		
		int generic_ptr_degree = 0;
		matches = 
				(test_uniqtype->contained[i_test_subobj].offset == cur_alloc_subobj_uniqtype->contained[i_obj_subobj].offset)
		 && (
				// exact match
				(test_uniqtype->contained[i_test_subobj].ptr
				 == cur_alloc_subobj_uniqtype->contained[i_obj_subobj].ptr)
				|| // loose match: if the test type has a generic ptr...
				(
					0 != (
						generic_ptr_degree = is_generic_pointer_type_of_degree_at_least(
							test_uniqtype->contained[i_test_subobj].ptr, 1)
					)
					&& pointer_has_degree(
						(struct uniqtype *) cur_alloc_subobj_uniqtype->contained[i_obj_subobj].ptr,
						generic_ptr_degree
					)
				)
				|| // loose match: signed/unsigned
				(UNIQTYPE_IS_BASE_TYPE(test_uniqtype->contained[i_test_subobj].ptr)
				 && UNIQTYPE_IS_BASE_TYPE(cur_alloc_subobj_uniqtype->contained[i_obj_subobj].ptr)
				 && 
				 (((struct uniqtype *) test_uniqtype->contained[i_test_subobj].ptr)->
				 	contained[0].ptr == ((struct uniqtype *) cur_alloc_subobj_uniqtype->contained[i_obj_subobj].ptr))
				)
		);
		if (!matches) goto try_deeper;
	}
	// if we terminated because we ran out of fields in the target type, fail
	if (i_test_subobj < test_uniqtype->nmemb) goto try_deeper;
	
	// if we got here, we succeeded
	goto loosely_like_a_succeeded;
	
	_Bool success;
	try_deeper:
		debug_printf(0, "No dice; will try the object type one level down if there is one...\n");
		success = __liballocs_first_subobject_spanning(
				&target_offset_within_alloc_uniqtype, &cur_alloc_subobj_uniqtype, &cur_containing_uniqtype,
				&cur_contained_pos);
		if (!success) goto loosely_like_a_failed;
		debug_printf(0, "... got %s\n", NAME_FOR_UNIQTYPE(cur_alloc_subobj_uniqtype));

	} while (1);
	
loosely_like_a_succeeded:
	if (test_uniqtype != alloc_uniqtype) debug_printf(0, "__loosely_like_a succeeded! test type %s, allocation type %s\n",
		NAME_FOR_UNIQTYPE(test_uniqtype), NAME_FOR_UNIQTYPE(alloc_uniqtype));
	++__libcrunch_succeeded;
	return 1;
	
	// if we got here, we've failed
	// if we got here, the check failed
loosely_like_a_failed:
	if (__currently_allocating || __currently_freeing) 
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		if (!is_suppressed(test_uniqtype->name, __builtin_return_address(0), alloc_uniqtype ? alloc_uniqtype->name : NULL))
		{
			if (should_report_failure_at(__builtin_return_address(0)))
			{
				debug_printf(0, "Failed check __loosely_like_a(%p, %p a.k.a. \"%s\") at %p (%s), allocation was a %s%s%s originating at %p\n", 
					obj, test_uniqtype, test_uniqtype->name,
					__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)),
					a ? a->name : "(no allocator)",
					(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
					NAME_FOR_UNIQTYPE(alloc_uniqtype), 
					alloc_site);
			}
		}
	}
	return 1; // HACK: so that the program will continue
}

int __is_a_pointer_of_degree_internal(const void *obj, int d)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	struct allocator *a = NULL;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype,
		&alloc_site);
	
	if (__builtin_expect(err != NULL, 0))
	{
		return 1;
	}
	
	signed target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 0 
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		target_offset_within_uniqtype %= alloc_uniqtype->pos_maxoff;
	}
	
	/* HACK */
	reinstate_looseness_if_necessary(alloc_start, alloc_site, alloc_uniqtype);
	
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype;
	
	if (holds_pointer_of_degree(alloc_uniqtype, d, target_offset_within_uniqtype))
	{
		++__libcrunch_succeeded;
		return 1;
	} // else goto ...
	
is_a_pointer_failed:
	++__libcrunch_failed;
	debug_printf(0, "Failed check __is_a_pointer_of_degree(%p, %d) at %p (%s), "
			"found an allocation of a %s%s%s "
			"originating at %p\n", 
		obj, d,
		__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)), 
		a ? a->name : "(no allocator)",
		(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
		NAME_FOR_UNIQTYPE(alloc_uniqtype),
		alloc_site);
	return 1; // so that program will continue
}

/* If we're writing into a non-generic pointer, 
 * __is_a(value, target's pointee type) must hold. It could hold at
 * any level in the stack of subobjects that "value" points into, so
 * we need the full __is_a check.
 * 
 * If we're writing into a generic pointer, we're more relaxed, but 
 * if target has degree 3, "value" must be the address of a degree2 pointer.
 */
struct match_cb_args
{
	struct uniqtype *type_of_pointer_being_stored_to;
	signed target_offset;
};
static int match_pointer_subobj_strict_cb(struct uniqtype *spans, signed span_start_offset, 
		unsigned depth, struct uniqtype *containing, struct contained *contained_pos, 
		signed containing_span_start_offset, void *arg)
{
	/* We're storing a pointer that is legitimately a pointer to t (among others) */
	struct uniqtype *t = spans;
	struct match_cb_args *args = (struct match_cb_args *) arg;
	struct uniqtype *type_we_can_store = UNIQTYPE_POINTEE_TYPE(args->type_of_pointer_being_stored_to);
	
	if (span_start_offset == args->target_offset && type_we_can_store == t)
	{
		return 1;
	}
	return 0;
}
static int match_pointer_subobj_generic_cb(struct uniqtype *spans, signed span_start_offset, 
		unsigned depth, struct uniqtype *containing, struct contained *contained_pos, 
		signed containing_span_start_offset, void *arg)
{
	/* We're storing a pointer that is legitimately a pointer to t (among others) */
	struct uniqtype *t = spans;
	struct match_cb_args *args = (struct match_cb_args *) arg;
	
	int degree_of_pointer_stored_to = pointer_degree(args->type_of_pointer_being_stored_to);

	if (span_start_offset == 0 && pointer_has_degree(t, degree_of_pointer_stored_to - 1))
	{
		return 1;
	}
	else return 0;
}
int __can_hold_pointer_internal(const void *obj, const void *value)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();

	/* To hold a pointer, we must be a pointer. Find the pointer subobject at `obj'. */
	struct allocator *obj_a = NULL;
	const void *obj_alloc_start;
	unsigned long obj_alloc_size_bytes;
	struct uniqtype *obj_alloc_uniqtype = (struct uniqtype *)0;
	const void *obj_alloc_site;
	
	struct liballocs_err *obj_err = __liballocs_get_alloc_info(obj, 
		&obj_a,
		&obj_alloc_start,
		&obj_alloc_size_bytes,
		&obj_alloc_uniqtype,
		&obj_alloc_site);
	
	if (__builtin_expect(obj_err != NULL, 0))
	{
		return 1;
	}
	
	signed obj_target_offset_within_uniqtype = (char*) obj - (char*) obj_alloc_start;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(obj_alloc_start, obj_alloc_site)
			&& obj_alloc_uniqtype
			&& obj_alloc_uniqtype->pos_maxoff != 0 
			&& obj_alloc_uniqtype->neg_maxoff == 0)
	{
		obj_target_offset_within_uniqtype %= obj_alloc_uniqtype->pos_maxoff;
	}
	
	struct uniqtype *cur_obj_uniqtype = obj_alloc_uniqtype;
	struct uniqtype *cur_containing_uniqtype = NULL;
	struct contained *cur_contained_pos = NULL;
	
	/* Descend the subobject hierarchy until we can't go any further (since pointers
	 * are atomic. */
	_Bool success = 1;
	while (success)
	{
		success = __liballocs_first_subobject_spanning(
			&obj_target_offset_within_uniqtype, &cur_obj_uniqtype, &cur_containing_uniqtype,
			&cur_contained_pos);
	}
	struct uniqtype *type_of_pointer_being_stored_to = cur_obj_uniqtype;
	
	struct allocator *value_a = NULL;
	const void *value_alloc_start = NULL;
	unsigned long value_alloc_size_bytes = (unsigned long) -1;
	struct uniqtype *value_alloc_uniqtype = (struct uniqtype *)0;
	const void *value_alloc_site = NULL;
	_Bool value_contract_is_specialisable = 0;
	
	/* Might we have a pointer? */
	if (obj_target_offset_within_uniqtype == 0 && UNIQTYPE_IS_POINTER_TYPE(cur_obj_uniqtype))
	{
		int d;
		struct uniqtype *ultimate_pointee_type;
		pointer_degree_and_ultimate_pointee_type(type_of_pointer_being_stored_to, &d, &ultimate_pointee_type);
		assert(d > 0);
		assert(ultimate_pointee_type);
		
		/* Is this a generic pointer, of zero degree? */
		_Bool is_generic = is_generic_ultimate_pointee(ultimate_pointee_type);
		if (d == 1 && is_generic)
		{
			/* We pass if the value as (at least) equal degree.
			 * Note that the value is "off-by-one" in degree: 
			 * if target has degree 1, any address will do. */
			++__libcrunch_succeeded;
			return 1;
		}
		
		/* If we got here, we're going to have to understand `value',
		 * whether we're generic or not. */
		
// 		if (is_generic_ultimate_pointee(ultimate_pointee_type))
// 		{
// 			/* if target has degree 2, "value" must be the address of a degree1 pointer.
// 			 * if target has degree 3, "value" must be the address of a degree2 pointer.
// 			 * Etc. */
// 			struct uniqtype *value_pointee_uniqtype
// 			 = __liballocs_get_alloc_type_innermost(value);
// 			assert(value_pointee_uniqtype);
// 			if (pointer_has_degree(value_pointee_uniqtype, d - 1))
// 			{
// 				++__libcrunch_succeeded;
// 				return 1;
// 			}
// 		}

		struct liballocs_err *value_err = __liballocs_get_alloc_info(value, 
			&value_a,
			&value_alloc_start,
			&value_alloc_size_bytes,
			&value_alloc_uniqtype,
			&value_alloc_site);

		if (__builtin_expect(value_err != NULL, 0)) return 1; // liballocs has already counted this abort
		
		signed value_target_offset_within_uniqtype = (char*) value - (char*) value_alloc_start;
		/* If we're searching in a heap array, we need to take the offset modulo the 
		 * element size. Otherwise just take the whole-block offset. */
		if (ALLOC_IS_DYNAMICALLY_SIZED(value_alloc_start, value_alloc_site)
				&& value_alloc_uniqtype
				&& value_alloc_uniqtype->pos_maxoff != 0 
				&& value_alloc_uniqtype->neg_maxoff == 0)
		{
			value_target_offset_within_uniqtype %= value_alloc_uniqtype->pos_maxoff;
		}
		/* HACK: preserve looseness of value. */
		reinstate_looseness_if_necessary(value_alloc_start, value_alloc_site, value_alloc_uniqtype);

		/* See if the top-level object matches */
		struct match_cb_args args = {
			.type_of_pointer_being_stored_to = type_of_pointer_being_stored_to,
			.target_offset = value_target_offset_within_uniqtype
		};
		int ret = (is_generic ? match_pointer_subobj_generic_cb : match_pointer_subobj_strict_cb)(
			value_alloc_uniqtype,
			0,
			0,
			NULL, NULL,
			0, 
			&args
		);
		/* Here we walk the subobject hierarchy until we hit 
		 * one that is at the right offset and equals test_uniqtype.
		 
		 __liballocs_walk_subobjects_starting(
		 
		 ) ... with a cb that tests equality with test_uniqtype and returns 
		 
		 */
		if (!ret) ret = __liballocs_walk_subobjects_spanning(value_target_offset_within_uniqtype, 
			value_alloc_uniqtype, 
			is_generic ? match_pointer_subobj_generic_cb : match_pointer_subobj_strict_cb, 
			&args);
		
		if (ret)
		{
			++__libcrunch_succeeded;
			return 1;
		}
	}
	/* Can we specialise the contract of
	 * 
	 *  either the written-to pointer
	 * or
	 *  the object pointed to 
	 *
	 * so that the check would succeed?
	 * 
	 * We can only specialise the contract of as-yet-"unused" objects.
	 * Might the written-to pointer be as-yet-"unused"?
	 * We know the check failed, so currently it can't point to the
	 * value we want it to, either because it's generic but has too-high degree
	 * or because it's non-generic and doesn't match "value".
	 * These don't seem like cases we want to specialise. The only one
	 * that makes sense is replacing it with a lower degree, and I can't see
	 * any practical case where that would arise (e.g. allocating sizeof void***
	 * when you actually want void** -- possible but weird).
	 * 
	 * Might the "value" object be as-yet-unused?
	 * Yes, certainly.
	 * The check failed, so it's the wrong type.
	 * If a refinement of its type yields a "right" type,
	 * we might be in business.
	 * What's a "right" type?
	 * If the written-to pointer is not generic, then it's that target type.
	 */
	// FIXME: use value_alloc_start to avoid another heap lookup
	struct insert *value_object_info = __liballocs_get_insert(value);
	/* HACK: until we have a "loose" bit */
	struct uniqtype *pointee = UNIQTYPE_POINTEE_TYPE(type_of_pointer_being_stored_to);

	if (!is_generic_pointer_type(type_of_pointer_being_stored_to)
		&& value_alloc_uniqtype
		&& UNIQTYPE_IS_POINTER_TYPE(value_alloc_uniqtype)
		&& is_generic_pointer_type(value_alloc_uniqtype)
		&& value_object_info
		&& STORAGE_CONTRACT_IS_LOOSE(value_object_info, value_alloc_site))
	{
		value_object_info->alloc_site_flag = 1;
		value_object_info->alloc_site = (uintptr_t) pointee; // i.e. *not* loose!
		debug_printf(0, "libcrunch: specialised allocation at %p from %s to %s\n", 
			value, NAME_FOR_UNIQTYPE(value_alloc_uniqtype), NAME_FOR_UNIQTYPE(pointee));
		++__libcrunch_lazy_heap_type_assignment;
		return 1;
	}

can_hold_pointer_failed:
	if (__currently_allocating || __currently_freeing)
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		debug_printf(0, "Failed check __can_hold_pointer(%p, %p) at %p (%s), "
				"target pointer is a %s, %ld bytes into a %s%s%s originating at %p, "
				"value points %ld bytes into a %s%s%s originating at %p\n", 
			obj, value,
			__builtin_return_address(0), format_symbolic_address(__builtin_return_address(0)), 
			NAME_FOR_UNIQTYPE(type_of_pointer_being_stored_to),
			(long)((char*) obj - (char*) obj_alloc_start),
			obj_a ? obj_a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(obj_alloc_start, obj_alloc_site) && obj_alloc_uniqtype && obj_alloc_size_bytes > obj_alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
			NAME_FOR_UNIQTYPE(obj_alloc_uniqtype),
			obj_alloc_site,
			(long)((char*) value - (char*) value_alloc_start),
			value_a ? value_a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(value_alloc_start, value_alloc_site) && value_alloc_uniqtype && value_alloc_size_bytes > value_alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
			NAME_FOR_UNIQTYPE(value_alloc_uniqtype),
			value_alloc_site
			);
	}
	return 1; // fail, but program continues
	
}

struct bounds_cb_arg
{
	struct uniqtype *passed_in_t;
	signed target_offset;
	_Bool success;
	struct uniqtype *matched_t;
	struct uniqtype *innermost_containing_array_t;
	signed innermost_containing_array_type_span_start_offset;
	struct uniqtype *outermost_containing_array_t;
	signed outermost_containing_array_type_span_start_offset;
	size_t accum_array_bounds;
};

static int bounds_cb(struct uniqtype *spans, signed span_start_offset, unsigned depth,
	struct uniqtype *containing, struct contained *contained_pos, 
	signed containing_span_start_offset, void *arg_void)
{
	struct bounds_cb_arg *arg = (struct bounds_cb_arg *) arg_void;

	/* If we've just descended through an object of array type, 
	 * remember this fact. This is so that we can calculate the
	 * whole-array bounds, if we're doing arithmetic on a 
	 * pointer to some element of an array of this type.
	 * 
	 * Also, for arrays of arrays, say int[][], 
	 * we actually want to range over the outermost bounds.
	 * This is not the case of arrays of structs of arrays.
	 * So we want to clear the state once we descend through a non-array. */
	if (UNIQTYPE_IS_ARRAY(containing))
	{
		arg->innermost_containing_array_type_span_start_offset
		 = containing_span_start_offset;
		arg->innermost_containing_array_t = containing;
		
		if (!arg->outermost_containing_array_t)
		{
			arg->outermost_containing_array_type_span_start_offset
			 = containing_span_start_offset;
			arg->outermost_containing_array_t = containing;
			arg->accum_array_bounds = UNIQTYPE_ARRAY_LEN(containing);
			if (arg->accum_array_bounds < 1) arg->accum_array_bounds = 0;
		}
		else arg->accum_array_bounds *= UNIQTYPE_ARRAY_LEN(containing);
	}
	else
	{
		arg->outermost_containing_array_type_span_start_offset = 0;
		arg->outermost_containing_array_t = NULL;
		arg->innermost_containing_array_type_span_start_offset = 0;
		arg->innermost_containing_array_t = NULL;
		arg->accum_array_bounds = 0;
	}
	
	if (span_start_offset < arg->target_offset)
	{
		return 0; // keep going
	}
	
	// now we have span_start_offset <= target_offset
	if (span_start_offset > arg->target_offset)
	{
		/* We've overshot. If this happens, it means the target offset
		 * is not a subobject start offset. This shouldn't happen,
		 * unless the caller makes a wild pointer. */
		return 1;
	}
	
	if (span_start_offset == arg->target_offset)
	{
		/* We've hit a subobject that starts at the right place.
		 * It might still be an enclosing object, not the object we're
		 * looking for. We differentiate using the size of the passed-in
		 * type -- this is the size of object that the pointer
		 * arithmetic is being done on. Keep going til we hit something
		 * of that size. */
		if (spans->pos_maxoff < arg->passed_in_t->pos_maxoff)
		{
			// usually shouldn't happen, but might with __like_a prefixing
			arg->success = 1;
			return 1;
		}
		if (spans->pos_maxoff > arg->passed_in_t->pos_maxoff)
		{
			// keep going
			return 0;
		}
		
		assert(spans->pos_maxoff == arg->passed_in_t->pos_maxoff);
		/* What are the array bounds? We don't have enough context,
		 * so the caller has to figure it out. */
		arg->success = 1;
		arg->matched_t = spans;

		return 1;
	}
	
	assert(0);
}

__libcrunch_bounds_t __fetch_bounds_internal(const void *obj, const void *derived, struct uniqtype *t)
{
	/* We might not be initialized yet (recall that __libcrunch_global_init is 
	 * not a constructor, because it's not safe to call super-early). */
	__libcrunch_check_init();
	
	if (!obj) goto return_min_bounds;
	
	struct uniqtype *test_uniqtype = t;
	struct allocator *a = NULL;
	const void *alloc_start;
	unsigned long alloc_size_bytes;
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0;
	const void *alloc_site;
	
	struct liballocs_err *err = __liballocs_get_alloc_info(obj, 
		&a,
		&alloc_start,
		&alloc_size_bytes,
		&alloc_uniqtype,
		&alloc_site);
	
	_Bool is_cacheable = 0;
	if (a) is_cacheable = a->is_cacheable;/* || ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)*/;
	
	if (__builtin_expect(err == &__liballocs_err_unrecognised_alloc_site, 0))
	{
		if (!(alloc_start && alloc_size_bytes)) goto abort_returning_max_bounds;
	}
	else if (__builtin_expect(err != NULL, 0))
	{
		goto abort_returning_max_bounds; // liballocs has already counted this abort
	}
	
	/* If we didn't get alloc site information, we might still have 
	 * start and size info. This can be enough for bounds checks. */
	if (alloc_start && alloc_size_bytes && !alloc_uniqtype)
	{
		/* Pretend it's a char allocation */
		alloc_uniqtype = pointer_to___uniqtype__signed_char;
	}
	
	if (t == pointer_to___uniqtype__signed_char
			|| t == pointer_to___uniqtype__unsigned_char)
	{
		goto return_alloc_bounds; // FIXME: this is C-specific
	}
	
	/* We can assume that the memory at obj is validly a t, for some "valid"
	 * (maybe __is_a, maybe __like_a, etc.).
	 * The question is how far that extends either side of obj. 
	 * Unfortunately we still have to do an __is_a-like search
	 * to establish our context. 
	 * AND we have to tolerate the various relationships -- is-a, like-a, etc.
	 * So, suppose we're doing arithmetic on an allocation of sockaddr, 
	 * but we actually get passed t as sockaddr_in, because that's what the caller
	 * is using. We won't even find sockaddr_in if we search in the alloc uniqtype.
	 * BUT WAIT. 
	 * What we're really asking about is the regularity of the memory around obj,
	 * when considered in strides of t->pos_maxoff. 
	 * It doesn't actually matter what t is.
	 * So:
	 * 
	 * - find the outermost uniqtype at offset obj - alloc_start
	 * - descend (offset zero) until we find something of *the same size or smaller than* 
	          t->pos_maxoff
	 * - if we find smaller, that means we used __like_a prefixing; bounds are only the pointee;
	 * - if we find equi-sized, use the bounds of the containing array if there is one.
	 * 
	 * If there's a nest of equi-sized objects, say struct { struct { } };, it doesn't matter.
	 * 
	 * Does this handle nested arrays of (arrays of) structs?
	 * 
	 */
	if (__builtin_expect( t->pos_maxoff == 65536 /* HACK test for -1 */ , 0))
	{
		goto return_min_bounds;
	}
	
	signed target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;
	char *alloc_instance_start_pos = (char*) alloc_start;
	unsigned short alloc_period = (alloc_uniqtype->neg_maxoff == 0 
			&& alloc_uniqtype->pos_maxoff > 0
			&& alloc_uniqtype->pos_maxoff != 65535 /* HACK */) ? alloc_uniqtype->pos_maxoff : 0;
	/* If we're searching in a heap array, we need to take the offset modulo the 
	 * element size. Otherwise just take the whole-block offset. */
	if (ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site)
			&& alloc_uniqtype
			&& alloc_uniqtype->pos_maxoff != 65535 /* HACK test for -1 */
			&& alloc_uniqtype->neg_maxoff == 0)
	{
		/* Test for regularity in the heap block itself. */
		alloc_instance_start_pos += alloc_uniqtype->pos_maxoff * 
			(target_offset_within_uniqtype / alloc_uniqtype->pos_maxoff);
		target_offset_within_uniqtype %= alloc_uniqtype->pos_maxoff;
		if (target_offset_within_uniqtype == 0)
		{
			// return the bounds of the heap block
			goto return_alloc_bounds;
		}
		// else keep going
	}
	struct bounds_cb_arg arg = {
		.passed_in_t = t,
		.target_offset = target_offset_within_uniqtype
	};
	int ret = __liballocs_walk_subobjects_spanning(
		target_offset_within_uniqtype,
		alloc_uniqtype, 
		bounds_cb, 
		&arg
	);
	if (arg.success)
	{
		if (arg.innermost_containing_array_t)
		{
			// bounds are the whole array
			const char *lower = alloc_instance_start_pos + arg.innermost_containing_array_type_span_start_offset;
			const char *upper = (arg.innermost_containing_array_t->array_len == 0) ? /* use the allocation's limit */ 
					alloc_start + alloc_size_bytes
					: alloc_instance_start_pos + arg.innermost_containing_array_type_span_start_offset
						+ (arg.innermost_containing_array_t->array_len * t->pos_maxoff);
			if (is_cacheable) cache_bounds(lower, upper, t, 1, alloc_uniqtype->pos_maxoff, alloc_start);
			return __make_bounds(
				(unsigned long) lower,
				(unsigned long) upper
			);
		}
		// bounds are just this object
		char *limit = (char*) obj + (t->pos_maxoff > 0 ? t->pos_maxoff : 1);
		if (is_cacheable) cache_bounds(obj, limit, t, 1, alloc_uniqtype->pos_maxoff, alloc_start);
		return __make_bounds((unsigned long) obj, (unsigned long) limit);
	}
	else
	{
		debug_printf(0, "libcrunch: no bounds for %p, target type %s, offset %d in allocation of %s at %p\n", 
			obj, NAME_FOR_UNIQTYPE(t), target_offset_within_uniqtype, NAME_FOR_UNIQTYPE(alloc_uniqtype),
			alloc_start);
		goto return_min_bounds;
	}

return_min_bounds:
	if (is_cacheable) cache_bounds(obj, (char*) obj + 1, NULL, 0, 1, alloc_start);
	return __make_bounds((unsigned long) obj, (unsigned long) obj + 1);

return_alloc_bounds:
	{
		char *base = (char*) alloc_start - alloc_uniqtype->neg_maxoff;
		char *limit = (char*) alloc_start + alloc_size_bytes;
		unsigned long size = limit - base;
		
		/* CHECK: do the bounds include the derived-from pointer? If not, we abort. */
		if ((unsigned long) obj - (unsigned long) base > size) goto abort_returning_max_bounds;
			
		if (is_cacheable) cache_bounds(base, 
			limit, 
			alloc_uniqtype, 
			alloc_uniqtype ? alloc_uniqtype->pos_maxoff : 1, 
			alloc_uniqtype ? alloc_uniqtype->pos_maxoff : 1,
			alloc_start);
		return __make_bounds(
			(unsigned long) base,
			(unsigned long) limit
		);
	}

abort_returning_max_bounds: 
	/* HACK: to avoid repeated slow-path queries for uninstrumented/unindexed 
	 * allocations, we cache a range of bytes here. 
	 * PROBLEM: if we currently need *bigger* bounds, we need some way to extend
	 * them, since we won't hit this path again. Otherwise we'll get bounds errors.
	 * Need to record that the bound are synthetic... use NULL alloc_base. */
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) < (y)) ? (y) : (x))
	cache_fake_bounds(
		MIN((char*) obj, (char*) derived), 
		MAX((char*) obj, (char*) derived) + (t ? t->pos_maxoff : 0), 
		t, 1, (t ? t->pos_maxoff : 1), NULL /* no alloc start */
	);
	
	debug_printf(0, "libcrunch: failed to fetch bounds for pointer %p (deriving %p); liballocs said %s (alloc site %p)\n", 
			obj, derived, err ? __liballocs_errstring(err) : "(found object did not include queried pointer)", alloc_site);
	return __libcrunch_max_bounds(obj);
}

void * __check_derive_ptr_internal(
		const void *derived, 
		const void *derivedfrom, 
		__libcrunch_bounds_t *derivedfrom_bounds, 
		struct uniqtype *t
)
{
	unsigned long t_sz = t->pos_maxoff;
	
	// deriving a null pointer or MAP_FAILED is okay, I suppose? don't allow it, for now
	// if (!derived || derived == (void*) -1) goto out;

	/* Note that if derivedfrom is really a trapvalue, its bounds should still be
	 * non-trapped, i.e. the actual object bounds. */
	
	/* ALSO note that we should always have a derivedfrom bounds *object*, because our
	 * instrumentation makes sure that pointer adjustments are local-to-local operations.
	 * If we load a pointer from the heap, then adjust it, it happens in two steps,
	 * and the latter is local-to-local. 
	 * What we might not have is valid derivedfrom bounds *information*. If the object
	 * has not been filled yet, it's our job to do it. */

	__libcrunch_bounds_t bounds;
	if (derivedfrom_bounds && !__libcrunch_bounds_invalid(*derivedfrom_bounds, derivedfrom))
	{
		bounds = *derivedfrom_bounds;
	}
	else
	{
		/* CARE: we could accept a "fetched from" parameter and try the 
		 * shadow space first. Since this is a slow path already, the
		 * inline fast-paths have presumably already tried and failed
		 * to fetch bounds this way, so we don't repeat the effort here. */
		bounds = __fetch_bounds_ool(derivedfrom, derived, t);
		if (derivedfrom_bounds) *derivedfrom_bounds = bounds;
	}
	
	if (unlikely(__libcrunch_ptr_trap_bits(derivedfrom) == LIBCRUNCH_TRAP_ONE_PAST))
	{
		/* de-trap derivedfrom */
		derivedfrom = __libcrunch_untrap(derivedfrom, LIBCRUNCH_TRAP_ONE_PAST);
	}
	
	unsigned long base = (unsigned long) __libcrunch_get_base(bounds, derivedfrom);
	unsigned long limit = (unsigned long) __libcrunch_get_limit(bounds, derivedfrom);
	unsigned long size = (unsigned long) __libcrunch_get_size(bounds, derivedfrom);
	unsigned long addr = (unsigned long) derived;
	
	// too low?
	//if (unlikely(addr < base)) { goto out_fail; }
	// NOTE: support for one-prev pointers as trap values goes here
	// too high?
	//if (unlikely(addr > limit)) { goto out_fail; }
	
	// We use Austin et al's "unsigned subtraction" hack here
	// which is (p292, Fig. 3)
	//if ((unsigned)(addr-base) > size - sizeof (<type>)) FlagSpatialError();
	if (addr - base > size) goto out_fail;

	// "one past"?
	if (addr == limit)
	{
		return __libcrunch_trap(derived, LIBCRUNCH_TRAP_ONE_PAST);
	}
	/* PROBLEM: if we're working with fake bounds, we might actually
	 * be trying to create a legitimate pointer. If we hit the (arbitrary)
	 * end of the fake bounds, we'll issue the trapped pointer and it will
	 * get hit. We could just deal with this in the segfault handler, but 
	 * ideally we'd instead never issue trapped pointers from fake bounds. 
	 * No amount of slack that we add to the fake bounds can guarantee this,
	 * however. We either have to test explicitly for fake bounds, which
	 * slows down our somewhat-fast path, or try to get a "rarely need
	 * segfault handler" probabilistic solution. Actually I think that
	 * explicitly testing for fake bounds is probably necessary. Do it
	 * when we call __fetch_bounds. Then get rid of the failure hack.
	 * 
	 * Note that we intend to re-inline the trap pointer handling. That
	 * makes things even more tricky. Can we make __fetch_bounds return
	 * a pointer to the cache entry? Split __fetch_bounds into
	 * __fetch_bounds_from_cache and __fetch_bounds_from_liballocs?
	 * 
	 * Okay, so on the secondary-and-slower paths we have 
	 * 
	 *    primary check
	 *    do we have local bounds? then do straight trap-pointer-or-fail
	 *    --------- inline path stops here?
	 *    else check the cache...
	 *       if we hit the ordinary cache, straight trap-pointer-or-fail
	 *                                        and write out the hit bounds
	 *    --------- or here?
	 *       if we hit the fake bounds cache, widen the fake bounds (never trap)
	 *                                        and write out max bounds
	 *    else if we miss the cache, 
	 *       fall back to liballocs
	 *            -- may create fake bounds here; again, write max bounds to local
	 * 
	 *    trap pointer checking (de-trap)? NO, we don't need to do that
	 *    trap pointer creation (maybe from local bounds)
	 *    cached bounds retrieval   
	 *     -- fake bounds handling, if we hit the fake bounds cache
	 *    invalid bounds handling a.k.a. liballocs bound lookup
	 */
	
	/* That's it! */
out:
	return (void*) derived;
out_fail:
	/* We might be failing because we locally cached some fake bounds that
	 * were not wide enough. Look in the cache. 
	 * AH. How do these get into localbounds? We were supposed to be returning
	 * max-bounds. But I suppose they can get in there.... */
	{
		__libcrunch_bounds_t from_cache = __fetch_bounds_from_cache(derivedfrom, derived,
			t, t_sz);
		if (!__libcrunch_bounds_invalid(from_cache, derivedfrom))
		{
			unsigned long new_base = (unsigned long) __libcrunch_get_base(from_cache, derivedfrom);
			unsigned long new_limit = (unsigned long) __libcrunch_get_limit(from_cache, derivedfrom);
			if (addr - new_base <= new_limit - new_base - t_sz) 
			{
				/* Okay, looks like we widened some fake the bounds. */
				debug_printf(1, "libcrunch: allowing derivation of %p (from %p) owing to cached fake bounds.\n", derived, derivedfrom);
				// FIXME: this violates our "valid bounds don't change" assumption
				//*derivedfrom_bounds = /* from_cache */
				//	__libcrunch_max_bounds(derivedfrom);
				return (void*) derived;
			}
		}
	}
	/* Don't fail here; print a warning and return a trapped pointer */
	__libcrunch_bounds_error_at(derived, derivedfrom, bounds, 
		__builtin_return_address(0));
	return __libcrunch_trap(derived, LIBCRUNCH_TRAP_INVALID);
}

void __libcrunch_bounds_error_at(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds, const void *addr)
{
	__libcrunch_check_init();
	
	warnx("code at %p generated an out-of-bounds pointer %p (from %p; difference %ld; lb %p; ub %p)",
		addr, derived, derivedfrom, 
		(char*) derived - (char*) derivedfrom, 
		__libcrunch_get_base(bounds, derivedfrom), 
		__libcrunch_get_limit(bounds, derivedfrom));
	++__libcrunch_created_invalid_pointer;
}

void __libcrunch_bounds_error(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds)
{
	__libcrunch_bounds_error_at(derived, derivedfrom, bounds, 
		__builtin_return_address(0));
}

__libcrunch_bounds_t 
(__attribute__((pure)) __fetch_bounds_ool)
(const void *ptr, const void *derived_ptr, struct uniqtype *t)
{
	++__libcrunch_fetch_bounds_called; // TEMP
	__libcrunch_bounds_t from_cache = __fetch_bounds_from_cache(
			ptr, derived_ptr, t, t->pos_maxoff
	);
	if (!__libcrunch_bounds_invalid(from_cache, ptr)) return from_cache;
	++__libcrunch_fetch_bounds_missed_cache; /* This should hardly ever happen!
	 * Only with uninstrumented code, or casts not on a recently malloc'd heap object,
	 * or fetching char* bounds, or GPP bounds. */
	return __fetch_bounds_internal(ptr, derived_ptr, t);
}
