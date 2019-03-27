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
#include <limits.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#ifdef USE_REAL_LIBUNWIND
#include <libunwind.h>
#endif
#include <errno.h>
#include "relf.h"
#include "libcrunch.h"
#include "libcrunch_private.h"

struct cache_miss_record {
	const void *caller;
	unsigned long count;
};
static int compare_miss_record_by_addr(const void *vmr1, const void *vmr2)
{
	struct cache_miss_record *mr1 = (void*) vmr1, *mr2 = (void*) vmr2;
	if (mr1->caller == mr2->caller) return 0;
	// NULL is the maximal value
	if (!mr1->caller) return 1;
	if (!mr2->caller) return -1;
	return ((uintptr_t) mr1->caller < (uintptr_t) mr2->caller) ? -1 : 1;
}
static struct cache_miss_record *miss_vec;
static unsigned long miss_vec_nused;
static unsigned long miss_vec_n;

int __libcrunch_get_errno(void) // for debugging
{
	return errno;
}

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

#ifndef MIN
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#endif
#ifndef MAX
#define MAX(x, y) (((x) < (y)) ? (y) : (x))
#endif

/* We need our own overriding versions of these, since the _stubs.so copies
 * won't be available to the preload object. */
void **__libcrunch_bounds_bases_region_00;
void **__libcrunch_bounds_bases_region_2a;
void **__libcrunch_bounds_bases_region_7a;
unsigned long *__libcrunch_bounds_sizes_region_00;
unsigned long *__libcrunch_bounds_sizes_region_2a;
unsigned long *__libcrunch_bounds_sizes_region_7a;

int __libcrunch_debug_level;
_Bool __libcrunch_is_initialized;
int __libcrunch_really_loaded; /* see shadow.c */

FILE *crunch_stream_err;// __attribute__((visibility("hidden")));

// helper
static const void *typestr_to_uniqtype_from_lib(void *handle, const char *typestr);

// HACK
void __libcrunch_preload_init(void);

/* Some data types like void* and sockaddr appear to be used to size a malloc(),
 * but are only used because they have the same size as the actual thing being
 * allocated (say a different type of pointer, or a family-specific sockaddr).
 * We keep a list of these. The user can use the LIBCRUNCH_ABSTRACT_LVALUE_TYPES
 * environment variable to add more. Note that this variable also affects how
 * trumptr does the instrumentation. It permits casts to (pointers to) these
 * abstract lvalue types, like (void**), but then checks *writes* to the lvalue
 * (i.e. a write to a void* lvalue -- so if we do *(void**)p, it's the write
 * and not the cast that gets checked). */
static unsigned abstract_lvalue_types_count;
static const char **abstract_lvalue_typenames;
static struct uniqtype **abstract_lvalue_types;

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
		t, NAME_FOR_UNIQTYPE(t), t->pos_maxoff);
	fflush(crunch_stream_err);
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
		debug_println(2, "dladdr() failed to find symbol for test site address %p", test_site);
		result = prefix_pattern_matches(pat, "");
	}
	return result;
}

static _Bool suppression_matches(struct suppression *s, 
		const char *test_typestr, const void *test_site, const char *alloc_typestr)
{
	return prefix_pattern_matches(s->test_type_pat, test_typestr)
			&& prefix_pattern_matches(s->alloc_type_pat, alloc_typestr)
			&& test_site_matches(s->testing_function_pat, test_site);
}

static _Bool is_suppressed(const struct uniqtype *maybe_test_uniqtype, const void *test_site, 
		const struct uniqtype *maybe_alloc_uniqtype)
{
	if (!suppressions) return 0;
	const char *test_typestr = maybe_test_uniqtype ? NAME_FOR_UNIQTYPE(maybe_test_uniqtype) : NULL;
	const char *alloc_typestr = maybe_alloc_uniqtype ? NAME_FOR_UNIQTYPE(maybe_alloc_uniqtype) : NULL;
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
	done_init = 1;
}

/* counters */
unsigned long __libcrunch_begun;
#ifdef LIBCRUNCH_EXTENDED_COUNTS
unsigned long __libcrunch_aborted_init;
unsigned long __libcrunch_trivially_succeeded;
#endif
unsigned long __libcrunch_aborted_typestr;
unsigned long __libcrunch_succeeded_by_specialization;
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
unsigned long __libcrunch_ptr_derivations;
unsigned long __libcrunch_ptr_derefs;
unsigned long __libcrunch_ptr_stores;

/* We maintain a *separate* cache of "fake bounds" that remember ranges 
 * over which we've let arithmetic go ahead because we had no type info
 * for the allocation. Without this, many programs will repeatedly report
 * failures getting bounds. We keep it separate to avoid interference with
 * the main cache. */
// FIXME: this should be thread-local but my gdb can't grok that
struct __liballocs_memrange_cache /* __thread */ __libcrunch_fake_bounds_cache = {
	.max_pos = 1 + LIBALLOCS_MEMRANGE_CACHE_MAX_SIZE,
	.next_victim = 1
};

static void report_repeat_failure_summary(void);
static unsigned long repeat_summarisation_count;
struct addrlist distinct_failure_sites;

/* This filter is used to avoid repeated warnings, unless 
 * the user has requested them (verbose mode). */
static _Bool should_report_failure_at(const void *site, const struct uniqtype *maybe_test_uniqtype,
	const struct uniqtype *maybe_alloc_uniqtype)
{
	if (is_suppressed(maybe_test_uniqtype, site, maybe_alloc_uniqtype)) return 0;
	if (verbose) return 1;
	_Bool is_unseen = !__liballocs_addrlist_contains(&distinct_failure_sites, (void*) site);
	if (is_unseen)
	{
		__liballocs_addrlist_add(&distinct_failure_sites, (void*) site);
		return 1;
	}
	else return 0;
}

static void print_exit_summary(void)
{
	if (__libcrunch_begun == 0 && __libcrunch_ptr_derefs == 0
		&& __libcrunch_fetch_bounds_called == 0 /* FIXME: replace with __fetch_bounds_internal failure counter */
		&& __libcrunch_created_invalid_pointer == 0
		&& !getenv("LIBCRUNCH_ALWAYS_PRINT_EXIT_SUMMARY")) return;
	
	report_repeat_failure_summary();
	
	fprintf(crunch_stream_err, "======================================================\n");
	fprintf(crunch_stream_err, "libcrunch summary: \n");
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "type checks begun:                         % 11ld\n", __libcrunch_begun);
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       aborted due to init failure:        % 11ld\n", __libcrunch_aborted_init);
#endif
	fprintf(crunch_stream_err, "       aborted for bad typename:           % 11ld\n", __libcrunch_aborted_typestr);
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       trivially passed:                   % 11ld\n", __libcrunch_trivially_succeeded);
#endif
#ifdef LIBCRUNCH_EXTENDED_COUNTS
	fprintf(crunch_stream_err, "       remaining                           % 11ld\n", __libcrunch_begun - (__libcrunch_trivially_succeeded + __liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr + __libcrunch_aborted_init));
#else
	fprintf(crunch_stream_err, "       remaining                           % 11ld\n", __libcrunch_begun - (__liballocs_aborted_unknown_storage + __libcrunch_aborted_typestr));
#endif	
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "   succeeded by specialization:            % 11ld\n", __libcrunch_succeeded_by_specialization);
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "       failed inside allocation functions: % 11ld\n", __libcrunch_failed_in_alloc);
	fprintf(crunch_stream_err, "       failed otherwise:                   % 11ld\n", __libcrunch_failed);
	fprintf(crunch_stream_err, "                 of which user suppressed: % 11ld\n", __libcrunch_failed_and_suppressed);
	fprintf(crunch_stream_err, "       nontrivially passed:                % 11ld\n", __libcrunch_succeeded);
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "   of which hit liballocs memrange cache:  % 11ld\n", __libcrunch_is_a_hit_cache);
#ifdef LIBCRUNCH_PROFILE_CACHE_MISSES
	fprintf(crunch_stream_err, "   miss profile:                           \n");
for (unsigned i = 0; i < miss_vec_nused; ++i)
{
	fprintf(crunch_stream_err, "   % 12lx                            % 11ld\n", (unsigned long) miss_vec[i].caller, miss_vec[i].count);
}
#endif
#ifdef LIBCRUNCH_KEEP_EXPENSIVE_COUNTS
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "pointer dereferences:                      % 11ld\n", __libcrunch_ptr_derefs);
	fprintf(crunch_stream_err, "   of which stored shadowed pointer values:% 11ld\n", __libcrunch_ptr_stores);
	fprintf(crunch_stream_err, "pointer derivations instrumented:          % 11ld\n", __libcrunch_ptr_derivations);
#endif
	fprintf(crunch_stream_err, "------------------------------------------------------\n");
	fprintf(crunch_stream_err, "out-of-bounds pointers created:            % 11ld\n", __libcrunch_created_invalid_pointer);
	fprintf(crunch_stream_err, "accesses trapped and emulated:             % 11ld\n", 0ul /* FIXME */);
	fprintf(crunch_stream_err, "calls to __fetch_bounds:                   % 11ld\n", __libcrunch_fetch_bounds_called /* FIXME: remove */);
	fprintf(crunch_stream_err, "   of which missed cache:                  % 11ld\n", __libcrunch_fetch_bounds_missed_cache);
	fprintf(crunch_stream_err, "calls requiring secondary checks           % 11ld\n", __libcrunch_primary_secondary_transitions);
	fprintf(crunch_stream_err, "trap-pointer fixups in fault handler       % 11ld\n", __libcrunch_fault_handler_fixups);
	fprintf(crunch_stream_err, "======================================================\n");
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
			MSGLIT("not a trap pointer\n");
			return;
		report:
			MSGLIT(" a ");
			MSGBUF(kindstr);
			const int shiftamount = 8*sizeof(uintptr_t) - LIBCRUNCH_TRAP_TAG_SHIFT;
			*p_savedval = (*p_savedval << shiftamount) >> shiftamount;
			MSGLIT(" trap pointer\n");
			MSGLIT("Attempting resume following de-trap; new value is ");
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

static void handle_sigbus(int signum, siginfo_t *info, void *ucontext_as_void)
{
	int dummy_ret;
	MSGLIT("*** libcrunch caught SIGBUS; sleeping for 10 seconds");
	sleep(10);
	raw_exit(128 + SIGBUS);
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
			debug_println(0, "could not open %s for writing", errvar);
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

int __libcrunch_global_init(void) __attribute__((constructor));
int __libcrunch_global_init(void)
{
	if (__libcrunch_is_initialized) return 0; // we are okay

	// don't try more than once to initialize
	static _Bool tried_to_initialize;
	if (tried_to_initialize) return -1;
	tried_to_initialize = 1;
	
	// we must have initialized liballocs
	__liballocs_ensure_init();
	
	/* We always include "void*" in the abstract lvalue types. (FIXME: this is a 
	 * C-specificity we'd rather not have here, but live with it for now.)
	 * We count the other ones. */
	const char *abstract_lvalue_types_str = getenv("LIBCRUNCH_ABSTRACT_LVALUE_TYPES");
	abstract_lvalue_types_count = 1;
	unsigned upper_bound = 2; // signed char plus one string with zero spaces
	if (abstract_lvalue_types_str)
	{
		unsigned count = count_separated_words(abstract_lvalue_types_str, ' ');
		upper_bound += count;
		abstract_lvalue_types_count += count;
	}
	/* Allocate and populate. */
	abstract_lvalue_typenames = calloc(upper_bound, sizeof (const char *));
	abstract_lvalue_types = calloc(upper_bound, sizeof (struct uniqtype *));

	// the first entry is always pointer-to-void
	abstract_lvalue_typenames[0] = "__PTR_void";
	if (abstract_lvalue_types_str)
	{
		fill_separated_words(&abstract_lvalue_typenames[1], abstract_lvalue_types_str, ' ',
				upper_bound - 1);
	}

	/* We have to look up the uniqtypes of abstract lvalue types.
	 * We used to walk the link map directly, like a debugger would
	 *                                           (like I always knew somebody should). */
	/* ... but that's slow. We just use the hash table. */
	for (unsigned i = 0; i < abstract_lvalue_types_count; ++i)
	{
		if (abstract_lvalue_typenames[i] && !abstract_lvalue_types[i])
		{
			// build the uniqtype name and use the power of the symbol hash tables
			char buf[4096];
			char *pos = &buf[0];
			strcpy(buf, "__uniqtype__"); // use the codeless version. FIXME: what if that's not enough?
			strncat(buf, abstract_lvalue_typenames[i], sizeof buf - sizeof "__uniqtype__");
			buf[sizeof buf - 1] = '\0';
			// look up in global namespace
			const void *u = dlsym(RTLD_DEFAULT, buf);
			// if we found it, install it
			if (u) abstract_lvalue_types[i] = (struct uniqtype *) u;
		}
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
				debug_println(1, "invalid suppression: %s", *p_word);
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

	debug_println(1, "libcrunch successfully initialized");
	
	return 0;
}

enum check_kind
{
	IS_A,
	LIKE_A,
	NAMED_A,
	IS_A_FUNCTION_REFINING,
	CHECK_ARGS,
	LOOSELY_LIKE_A,
	IS_A_POINTER_OF_DEGREE,
	CAN_HOLD_POINTER,
	DERIVED_PTR_VALID,
	DEREFED_PTR_VALID,
	CHECK_MAX = 0x255
};
static const char *check_kind_names[CHECK_MAX + 1] = {
	[IS_A]              = "__is_a",
	[LIKE_A]            = "__like_a",
	[NAMED_A]           = "__named_a",
	[IS_A_FUNCTION_REFINING] = "__is_a_function_refining",
	[CHECK_ARGS]        = "__check_args",
	[LOOSELY_LIKE_A]    = "__loosely_like_a",
	[IS_A_POINTER_OF_DEGREE] = "__is_a_pointer_of_degree",
	[CAN_HOLD_POINTER] = "__can_hold_pointer",
	[DERIVED_PTR_VALID] = "__check_derive_ptr",
	[DEREFED_PTR_VALID] = "__check_deref_ptr"
};

static struct
{
	enum check_kind kind;
	const void *site;
	const void *opaque_decider;
} last_failed_check;
static unsigned long repeat_failure_suppression_count;

static void report_repeat_failure_summary(void)
{
	if (repeat_summarisation_count > 0)
	{
		debug_println(0, "Saw %ld further occurrences of the previous error",
				repeat_summarisation_count);
		repeat_summarisation_count = 0;
	}
}

#define report_failure_if_necessary(site, kind, found_uniqtype, test_uniqtype, \
	alloc_uniqtype, fmt, args...) \
	do { if (should_report_failure_at(site, test_uniqtype, alloc_uniqtype)) \
		report_failure(site, kind, found_uniqtype, test_uniqtype, alloc_uniqtype, fmt, ## args); \
	} while (0)

static void report_failure(const void *site,
	enum check_kind kind,
	const struct uniqtype *found_uniqtype,
	const struct uniqtype *test_uniqtype,
	const struct uniqtype *alloc_uniqtype,
	const char *fmt,
	...)
{
	va_list ap;
	va_start(ap, fmt);
	if (last_failed_check.site == site
			&& last_failed_check.opaque_decider /* deepest_subobject_type */ == found_uniqtype)
	{
		++repeat_summarisation_count;
	}
	else
	{
		report_repeat_failure_summary();
		debug_printf(0, "Failed check at %p (%s): %s", site,
			format_symbolic_address(site), check_kind_names[kind]);
		debug_vprintf_nohdr(0, fmt, ap);
		debug_printf_bare(0, "\n");
		last_failed_check.kind = kind;
		last_failed_check.site = site;
		// FIXME: the choice of decider depends on the check kind
		last_failed_check.opaque_decider /* deepest_subobject_type */ = found_uniqtype;
	}
	va_end(ap);
}

static void cache_bounds(const void *range_base, const void *range_limit, unsigned period,
	unsigned offset_to_t, const struct uniqtype *t, unsigned short depth)
{
	__liballocs_cache_with_type(&__liballocs_ool_cache,
		range_base, range_limit, period, offset_to_t, t, depth);
}
static void cache_is_a(const void *range_base, const void *range_limit, unsigned period,
	unsigned offset_to_t, const struct uniqtype *t, unsigned short depth)
{
	__liballocs_cache_with_type(&__liballocs_ool_cache,
		range_base, range_limit, period, offset_to_t, t, depth);
}
static void cache_fake_bounds(const void *range_base, const void *range_limit, unsigned period,
	unsigned offset_to_t, const struct uniqtype *t, unsigned short depth)
{
	debug_println(1, "Creating fake bounds %p-%p", range_base, range_limit);
	__liballocs_cache_with_type(&__libcrunch_fake_bounds_cache,
		range_base, range_limit, period,
		0, t, depth);
}

void __ensure_bounds_in_cache(unsigned long ptrval, __libcrunch_bounds_t ptr_bounds, struct uniqtype *t);
void __ensure_bounds_in_cache(unsigned long ptrval, __libcrunch_bounds_t ptr_bounds, struct uniqtype *t)
{
	/* We ensure in crunchbound.ml that ptr is never trapped.
	 * It follows that it might be pointing one-past-the-end... FIXME: do we deal with that? */
	const void *ptr = (const void *) ptrval;
	/* Never cache invalid bounds or max bounds. */
	if (__libcrunch_bounds_invalid(ptr_bounds, ptr)) return;
	if (__libcrunch_get_limit(ptr_bounds, ptr) == (void*) -1) return;
	__libcrunch_bounds_t from_cache = __fetch_bounds_from_cache(ptr, ptr, t, t->pos_maxoff);
	if (__libcrunch_bounds_invalid(from_cache, ptr))
	{
		cache_bounds(__libcrunch_get_base(ptr_bounds, ptr), __libcrunch_get_limit(ptr_bounds, ptr),
				t->pos_maxoff, 0, t, 1 /* HACK FIXME */);
	}
}

#define CHECK_INIT \
	/* When a check is called, we might not be initialized yet */ \
	/* (recall that __libcrunch_global_init is not a constructor, */ \
	/* because it's not safe to call super-early). */ \
__libcrunch_check_init();

#define DO_QUERY(obj) \
	__libcrunch_check_init(); \
	struct allocator *a = NULL; \
	const void *alloc_start; \
	unsigned long alloc_size_bytes; \
	struct uniqtype *alloc_uniqtype = (struct uniqtype *)0; \
	const void *alloc_site; \
	 \
	struct liballocs_err *err = __liballocs_get_alloc_info(obj,  \
		&a, \
		&alloc_start, \
		&alloc_size_bytes, \
		&alloc_uniqtype, \
		&alloc_site); \
	 \
	if (__builtin_expect(err != NULL, 0)) goto out; /* liballocs has already counted this abort */ \

#define DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, allocator) \
	if (alloc_uniqtype && alloc_uniqtype->make_precise) \
	{ \
		/* HACK: special-case to avoid overheads of 1-element array type creation */ \
		if (alloc_uniqtype->make_precise == __liballocs_make_array_precise_with_memory_bounds && \
			1 == (alloc_size_bytes / alloc_uniqtype->pos_maxoff)) \
		{ \
			alloc_uniqtype = UNIQTYPE_ARRAY_ELEMENT_TYPE(alloc_uniqtype); \
		} \
		else \
		{ \
			/* FIXME: should really do a fuller treatment of make_precise, to allow e.g. */ \
			/* returning a fresh uniqtype into a buffer, and (even) passing mcontext. */ \
			alloc_uniqtype = alloc_uniqtype->make_precise(alloc_uniqtype, \
				NULL, 0, (void*) alloc_start, (void*) alloc_start, alloc_size_bytes, \
				__builtin_return_address(0), NULL); \
		} \
		/* Now ask the meta-alloc protocol to update that object's metadata to this type. */ \
		if (allocator && allocator->set_type) allocator->set_type(NULL, (void*) alloc_start, alloc_uniqtype); \
	}

#define INIT_SUBOBJ_SEARCH(obj) \
	struct uniqtype *cur_obj_uniqtype = alloc_uniqtype; \
	struct uniqtype *cur_containing_uniqtype = NULL; \
	struct uniqtype_rel_info *cur_contained_pos = NULL; \
	unsigned cumulative_offset_searched = 0; \
	unsigned target_offset_within_uniqtype = (char*) obj - (char*) alloc_start;

struct starts_at_target_offset_and_has_type_nocache_args {
	unsigned target_offset;
	struct uniqtype *test_type;
};
static _Bool starts_at_target_offset_and_has_type_nocache(struct uniqtype *u,
	struct uniqtype_containment_ctxt *ucc,
	unsigned u_offset_from_search_start,
	void *args_as_void)
{
	struct starts_at_target_offset_and_has_type_nocache_args *args
	 = (struct starts_at_target_offset_and_has_type_nocache_args *) args_as_void;
	return args->target_offset == u_offset_from_search_start
			&& u == args->test_type;
}
struct starts_at_target_offset_and_has_type_args {
	struct starts_at_target_offset_and_has_type_nocache_args nocache_args;
	uintptr_t alloc_base;
	struct allocator *a;
	struct uniqtype *out_cur_obj_uniqtype;
	unsigned out_offset_from_search_start;
};
#define MAX_TO_CACHE 1
static inline unsigned chain_cache_toplevel(
		struct uniqtype *cur_u,
		uintptr_t cur_u_addr,
		struct uniqtype_containment_ctxt *cur_ucc,
		uintptr_t initial_u_addr,
		struct starts_at_target_offset_and_has_type_nocache_args *nocache_args)
{
	/* First we always recurse up to the top. */
	unsigned n_so_far = 0;
	if (cur_ucc->next) return chain_cache_toplevel(
		cur_ucc->u_container,
		cur_u_addr - cur_ucc->u_offset_within_container,
		cur_ucc->next,
		initial_u_addr,
		nocache_args
	);
	// we're toplevel; we definitely cache something
	if (UNIQTYPE_IS_ARRAY_TYPE(cur_u))
	{
		/* We now have several potentially distinct types in play.
		  - "t" is the initial u, i.e. the test type.
		  - the array type cur_u
		  - the array element type. */
		assert(UNIQTYPE_HAS_KNOWN_LENGTH(cur_u));
		struct uniqtype *array_element_type = UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_u);
		uintptr_t array_base = cur_u_addr;
		uintptr_t array_end = cur_u_addr + cur_u->pos_maxoff;
		cache_is_a((char*) array_base,
			/* range_limit */ (char*) array_end,
			/* period */ array_element_type->pos_maxoff,
			/* we cache the fact that it contains <test_uniqtype>s, not <array element type>s! */
			/* offset_to_a_t */ (initial_u_addr - cur_u_addr) % array_element_type->pos_maxoff,
			/* t */ nocache_args->test_type,
			/* depth */ 1 /* HACK FIXME */);
		return 1;
	}
	// didn't find an array; or we may be a top-level object. Just cache that object
	cache_is_a(/* range_base */ (char*) cur_u_addr,
		/* range_limit */ (char*) cur_u + cur_u->pos_maxoff,
		/* period */ nocache_args->test_type->pos_maxoff,
		/* offset_to_a_t */ initial_u_addr - cur_u_addr, //cur_contained_pos->un.memb.off,
		/* t */ nocache_args->test_type,
		/* depth */ 1 /* HACK FIXME */);
	return 1;
}

static inline unsigned chain_consider_caching_array(
		struct uniqtype *cur_u,
		uintptr_t cur_u_addr,
		struct uniqtype_containment_ctxt *cur_ucc,
		uintptr_t initial_u_addr,
		struct starts_at_target_offset_and_has_type_nocache_args *nocache_args)
{
	/* First we always recurse up to the top. */
	unsigned n_so_far = 0;
	if (cur_ucc->next) n_so_far = chain_consider_caching_array(
		cur_ucc->u_container,
		cur_u_addr - cur_ucc->u_offset_within_container,
		cur_ucc->next,
		initial_u_addr,
		nocache_args
	);
	if (n_so_far != MAX_TO_CACHE)
	{
		/* Now we can consider caching ourselves. Don't cache the test type, though. */
		if (UNIQTYPE_IS_ARRAY_TYPE(cur_u) && cur_u != nocache_args->test_type)
		{
			/* We now have several potentially distinct types in play.
			  - "t" is the initial u, i.e. the test type.
			  - the array type cur_u
			  - the array element type. */
			assert(UNIQTYPE_HAS_KNOWN_LENGTH(cur_u));
			struct uniqtype *array_element_type = UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_u);
			uintptr_t array_base = cur_u_addr;
			uintptr_t array_end = cur_u_addr + cur_u->pos_maxoff;
			cache_is_a((char*) array_base,
				/* range_limit */ (char*) array_end,
				/* period */ array_element_type->pos_maxoff,
				/* we cache the fact that it contains <test_uniqtype>s, not <array element type>s! */
				/* offset_to_a_t */ (initial_u_addr - cur_u_addr) % array_element_type->pos_maxoff,
				/* t */ nocache_args->test_type,
				/* depth */ 1 /* HACK FIXME */);
			return n_so_far + 1;
		}
	}
	return n_so_far;
}

static _Bool starts_at_target_offset_and_has_type(struct uniqtype *u,
	struct uniqtype_containment_ctxt *ucc,
	unsigned u_offset_from_search_start,
	void *args_as_void)
{
	struct starts_at_target_offset_and_has_type_args *args
	 = (struct starts_at_target_offset_and_has_type_args *) args_as_void;
	args->out_cur_obj_uniqtype = u;
	args->out_offset_from_search_start = u_offset_from_search_start;
	_Bool can_stop_now = starts_at_target_offset_and_has_type_nocache(u,
		ucc, u_offset_from_search_start, &args->nocache_args);
	if (can_stop_now && __builtin_expect(args->a && args->a->is_cacheable, 1))
	{
		assert(u == args->nocache_args.test_type);
		/* We walked (potentially) a ton of objects. What memranges do we want to cache?
		 * One answer    : (1) the target object/type and its nearest enclosing array.
		 * Another answer: (2) the target object/type and its outermost enclosing array.
		 * Another answer: (3) the target object/type and all arrays that enclose it.
		 * Another answer: (4) the target object/type whether or not it's in an array.
		 * Another answer: (5) all of the above.
		 * Another answer: (6) the target object and all enclosing objects.
		 * In terms of value for cache entries, it seems choosing an outermore array
		 * is better -- it covers more memory (but perhaps only a narrow slice of that
		 * memory).
		 * We should also do (4) in the case where there is no enclosing array.
		 */
		// walk to the outermost containment context, then walk down looking for an array
		uintptr_t initial_u_addr = args->alloc_base + args->nocache_args.target_offset;
		unsigned ncached = /*chain_consider_caching_array*/ chain_cache_toplevel(
			u,
			initial_u_addr,
			ucc,
			initial_u_addr,
			&args->nocache_args
		);
// 		unsigned ncached = 0;
// 		// the *cast target* subobject could in principle be an array type
// 		// ... but it is not the array type we want. So start looking one level up.
// 		if (ucc && ucc->next)
// 		{
// 			// we start with considering the container and *its* containment context
// 			struct uniqtype *cur_u = ucc->u_container;
// 			uintptr_t cur_u_addr = initial_u_addr - ucc->u_offset_within_container;
// 			struct uniqtype_containment_ctxt *c = ucc->next;
// 			while (c)
// 			{
// 				if (UNIQTYPE_IS_ARRAY_TYPE(cur_u))
// 				{
// 					/* We now have several potentially distinct types in play.
// 					  - "t" is the initial u, i.e. the test type.
// 					  - the array type cur_u
// 					  - the array element type. */
// 					assert(UNIQTYPE_HAS_KNOWN_LENGTH(cur_u));
// 					struct uniqtype *array_element_type = UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_u);
// 					uintptr_t array_base = cur_u_addr;
// 					uintptr_t array_end = cur_u_addr + cur_u->pos_maxoff;
//
// 					cache_is_a((char*) array_base,
// 						/* range_limit */ (char*) array_end,
// 						/* period */ array_element_type->pos_maxoff,
// 						/* we cache the fact that it contains <test_uniqtype>s, not <array element type>s! */
// 						/* offset_to_a_t */ (initial_u_addr - cur_u_addr) % array_element_type->pos_maxoff,
// 						/* t */ args->nocache_args.test_type,
// 						/* depth */ 1 /* HACK FIXME */);
// 					++ncached;
// 					if (ncached == MAX_TO_CACHE) break;
// 				}
//
// 			next:
// 				cur_u_addr -= c->u_offset_within_container;
// 				cur_u = c->u_container;
// 				c = c->next;
// 			}
// 		}
		if (ncached < MAX_TO_CACHE)
		{
			// didn't find an array; or we may be a top-level object. Just cache that object
			cache_is_a(/* range_base */ (char*) args->alloc_base + args->nocache_args.target_offset,
				/* range_limit */ (char*) args->alloc_base + args->nocache_args.target_offset + args->nocache_args.test_type->pos_maxoff,
				/* period */ 0,
				/* offset_to_a_t */ 0, //cur_contained_pos->un.memb.off,
				/* t */ args->nocache_args.test_type,
				/* depth */ 1 /* HACK FIXME */);
		}
	}
	return can_stop_now;
}

static void profile_cache_miss(const void *obj, const void *arg, const void *caller)
{
#ifdef LIBCRUNCH_PROFILE_CACHE_MISSES
	if (!miss_vec)
	{
		miss_vec_n = 10000;
		miss_vec = calloc(miss_vec_n, sizeof (struct cache_miss_record));
	}
	if (!miss_vec) abort();

	/* Look for our miss site */
	if (miss_vec_nused > 0)
	{
		struct cache_miss_record key_dummy = (struct cache_miss_record) { .caller = caller };
		struct cache_miss_record *found = bsearch(&key_dummy, miss_vec,
			miss_vec_nused, sizeof (struct cache_miss_record), compare_miss_record_by_addr);
		if (found)
		{
			++(found->count);
			return;
		}
	}
	// we need to add a new entry, perhaps by growing the array
	if (miss_vec_nused == miss_vec_n)
	{
		miss_vec_n *= 2;
		miss_vec = realloc(miss_vec, miss_vec_n * sizeof (struct cache_miss_record));
	}
	assert(miss_vec_nused < miss_vec_n);
	miss_vec[miss_vec_nused++] = (struct cache_miss_record) {
		.caller = caller,
		.count = 1
	};
	qsort(miss_vec, miss_vec_nused, sizeof (struct cache_miss_record), compare_miss_record_by_addr);
#endif
}

static _Bool try_specialize_memrange_type(
						struct uniqtype *current_type,
						const void *range_start,
						size_t range_len_bytes,
						struct uniqtype *specialized_type_or_element_type,
						struct allocator *a,
						const void *containing_alloc_start,
						struct uniqtype *containing_alloc_type);

int __is_a_internal(const void *obj, const void *arg)
{
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	profile_cache_miss(obj, arg, __builtin_return_address(0));
	unsigned containing_instance_offset = 0;
	CHECK_INIT
	DO_QUERY(obj)
	INIT_SUBOBJ_SEARCH(obj)
	_Bool success;
	struct starts_at_target_offset_and_has_type_args args = {
		.nocache_args = (struct starts_at_target_offset_and_has_type_nocache_args) {
			.test_type = (struct uniqtype *) test_uniqtype,
			.target_offset = target_offset_within_uniqtype
		},
		.alloc_base = (uintptr_t) alloc_start,
		.a = a
	};
	/* Optimisation: if we have an array whose element type is smaller than the
	 * test type (e.g. an array of uninterpreted bytes or chars),
	 * we're never going to find an instance of the test type within it,
	 * we can skip precisify and fail. Or if we're zero-offset and the types
	 * match, we can skip and pass. */
	// FIXME: these should fall out naturally from our search
	if (UNIQTYPE_IS_ARRAY_TYPE(alloc_uniqtype) &&
		UNIQTYPE_ARRAY_ELEMENT_TYPE(alloc_uniqtype)
		&& UNIQTYPE_ARRAY_ELEMENT_TYPE(alloc_uniqtype)->pos_maxoff
			< test_uniqtype->pos_maxoff)
	{
		goto fail;
	}
	else
	{
		DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
		cur_obj_uniqtype = alloc_uniqtype; /* HACK: necessary because may be updated by precisify */
		success = __liballocs_search_subobjects_spanning(
			/* u */ alloc_uniqtype,
			/* target_offset_within_u */ (char*) obj - (char*) alloc_start,
			/* visit_stop_test */ starts_at_target_offset_and_has_type,
			/* arg */ &args,
			/* out_offset */ NULL,
			/* out_ctxt */ NULL);
		/* We just walked a ton of objects. What memranges do we want to cache?
		 * One answer    : (1) the target object/type and its nearest enclosing array.
		 * Another answer: (2) the target object/type and its outermost enclosing array.
		 * Another answer: (3) the target object/type and all arrays that enclose it.
		 * Another answer: (4) the target object/type whether or not it's in an array.
		 * Another answer: (5) all of the above.
		 * Another answer: (6) the target object and all enclosing objects.
		 * In terms of value for cache entries, it seems (1) is best.
		 * But also (4) in the case where there is no enclosing array.
		 * PROBLEM: we want to be able to walk the up the containment contexts,
		 * at the callback site, so that we can find arrays. Currently our callback
		 * doesn't give us that. */
	}

	if (__builtin_expect(success, 1))
	{
		++__libcrunch_succeeded;
		return 1;
	}
	
	/* If we got here, we have not yet succeeded. But we might still do so, if we
	 * can specialize the object to the type we want.
	 * Complication: what type do we match?
	 * We used to match on alloc_uniqtype, which seems wrong.
	 * AH, but it has to be so, because we're updating the whole chunk's type info.
	 * If a heap block is an __ARRn of X, and X is abstract, we want to match X.
	 * But we might have terminated on a subobject of X.
	 */
	if (0 == ((char*) obj - (char*) alloc_start) % UNIQTYPE_SIZE_IN_BYTES(test_uniqtype))
	{
		success = try_specialize_memrange_type(
			alloc_uniqtype,
			alloc_start,
			alloc_size_bytes,
			(struct uniqtype *) test_uniqtype,
			a,
			alloc_start,
			alloc_uniqtype);
		if (success) { ++__libcrunch_succeeded_by_specialization; return 1; }
	}

	// we no longer cache negative results, since hopefully they're rare
fail:
	if (__currently_allocating || __currently_freeing)
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		report_failure_if_necessary(__builtin_return_address(0),
			IS_A,
			cur_obj_uniqtype,
			test_uniqtype,
			alloc_uniqtype,
			"(%p, %p a.k.a. \"%s\"): "
				"ptr is %ld bytes into a %s%s%s "
				"(deepest subobject spanning ptr: %s at offset %d) "
				"originating at %p\n", 
			obj, test_uniqtype, NAME_FOR_UNIQTYPE(test_uniqtype),
			(long)((char*) obj - (char*) alloc_start),
			a ? a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ",
			NAME_FOR_UNIQTYPE(alloc_uniqtype),
			(args.out_cur_obj_uniqtype ?
				((args.out_cur_obj_uniqtype == alloc_uniqtype) ? "(the same)" : NAME_FOR_UNIQTYPE(args.out_cur_obj_uniqtype))
				: "(none)"),
			(int) args.out_offset_from_search_start,
			(void*) alloc_site
		);
	}
out:
	return 1; // HACK: so that the program will continue
}

int __like_a_internal(const void *obj, const void *arg)
{
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	
	CHECK_INIT
	DO_QUERY(obj)
	DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
	INIT_SUBOBJ_SEARCH(obj)
	/* We don't update these, nor need them in this function, so
	 * null them to avoid surprises. */
	cur_containing_uniqtype = NULL;
	cur_contained_pos = NULL;
	
	/* Descend the subobject hierarchy until our target offset is zero, i.e. we 
	 * find the outermost thing in the subobject tree that starts at the address
	 * we were passed (obj). FIXME: doesn't seem quite right; what if we want
	 * to unwrap one more level? */
	unsigned distance_traversed;
	cur_obj_uniqtype = __liballocs_outermost_subobject_at_offset(cur_obj_uniqtype,
		target_offset_within_uniqtype, &distance_traversed);
	if (!cur_obj_uniqtype) goto like_a_failed;
	target_offset_within_uniqtype -= distance_traversed;
	assert(target_offset_within_uniqtype == 0);
	/* That's not quite enough: we want a *non-array* (we assume the test uniqtype
	 * is never an array uniqtype... FIXME: is that reasonable? *seems* okay...) */
	while (UNIQTYPE_IS_ARRAY_TYPE(cur_obj_uniqtype))
	{
		cur_obj_uniqtype = UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_obj_uniqtype);
	}
	
	// trivially, identical types are like one another
	if (test_uniqtype == cur_obj_uniqtype) goto like_a_succeeded;
	
	// arrays are special
	_Bool matches;
	if (__builtin_expect((UNIQTYPE_IS_ARRAY_TYPE(cur_obj_uniqtype)
			|| UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype)), 0))
	{
		matches = 
			test_uniqtype == cur_obj_uniqtype
		||  (UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype) && UNIQTYPE_ARRAY_LENGTH(test_uniqtype) == 1
				&& UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype) == cur_obj_uniqtype)
		||  (UNIQTYPE_IS_ARRAY_TYPE(cur_obj_uniqtype) && UNIQTYPE_ARRAY_LENGTH(cur_obj_uniqtype) == 1
				&& UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_obj_uniqtype) == test_uniqtype);
		/* We don't need to allow an array of one blah to be like a different
		 * array of one blah, because they should be the same type. 
		 * FIXME: there's a difficult case: an array of statically unknown length, 
		 * which happens to have length 1. */
		if (matches) goto like_a_succeeded; else goto like_a_failed;
	}
	
	/* We might have base types with signedness complements. */
	if (UNIQTYPE_IS_BASE_TYPE(cur_obj_uniqtype) && UNIQTYPE_IS_BASE_TYPE(test_uniqtype))
	{
		/* Does the cur obj type have a signedness complement matching the test type? */
		if (UNIQTYPE_BASE_TYPE_SIGNEDNESS_COMPLEMENT(cur_obj_uniqtype) == test_uniqtype) goto like_a_succeeded;
		/* Does the test type have a signedness complement matching the cur obj type? */
		if (UNIQTYPE_BASE_TYPE_SIGNEDNESS_COMPLEMENT(test_uniqtype) == cur_obj_uniqtype) goto like_a_succeeded;
	}
	
	/* Okay, we can start the like-a test: for each element in the test type, 
	 * do we have a type-equivalent in the object type?
	 * 
	 * We make an exception for arrays of char (signed or unsigned): if an
	 * element in the test type is such an array, we skip over any number of
	 * fields in the object type, until we reach the offset of the end element.  */
	unsigned i_obj_subobj = 0, i_test_subobj = 0;
	for (; 
		i_obj_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype)
			 && i_test_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype); 
		++i_test_subobj, ++i_obj_subobj)
	{
		if (__builtin_expect(UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
			&& (UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
					== pointer_to___uniqtype__signed_char
			|| UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
					== pointer_to___uniqtype__unsigned_char), 0))
		{
			// we will skip this field in the test type
			unsigned target_off =
				UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype) > i_test_subobj + 1
			 ?  test_uniqtype->related[i_test_subobj + 1].un.memb.off
			 :  test_uniqtype->related[i_test_subobj].un.memb.off
			      + test_uniqtype->related[i_test_subobj].un.memb.ptr->pos_maxoff;
			
			// ... if there's more in the test type, advance i_obj_subobj
			while (i_obj_subobj + 1 < UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype) &&
				cur_obj_uniqtype->related[i_obj_subobj + 1].un.memb.off
					< target_off) ++i_obj_subobj;
			/* We fail if we ran out of stuff in the target object type
			 * AND there is more to go in the test type. */
			if (i_obj_subobj + 1 >= UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype)
			 && UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype) > i_test_subobj + 1) goto like_a_failed;
				
			continue;
		}
		matches = 
				test_uniqtype->related[i_test_subobj].un.memb.off == cur_obj_uniqtype->related[i_obj_subobj].un.memb.off
		 && 	test_uniqtype->related[i_test_subobj].un.memb.ptr == cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr;
		if (!matches) goto like_a_failed;
	}
	// if we terminated because we ran out of fields in the target type, fail
	if (i_test_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype)) goto like_a_failed;
	
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
		report_failure_if_necessary(__builtin_return_address(0),
			LIKE_A,
			cur_obj_uniqtype,
			test_uniqtype,
			alloc_uniqtype,
			"(%p, %p a.k.a. \"%s\"): allocation was a %s%s%s originating at %p", 
				obj, test_uniqtype, NAME_FOR_UNIQTYPE(test_uniqtype),
				a ? a->name : "(no allocator)",
				(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
				NAME_FOR_UNIQTYPE(alloc_uniqtype), 
				alloc_site);
	}
out:
	return 1; // HACK: so that the program will continue
}

struct offset_and_name_args
{
	unsigned offset;
	const char *name;
};
static _Bool offset_and_name_match_cb(struct uniqtype *u, struct uniqtype_containment_ctxt *ucc,
	unsigned u_offset_from_search_start, void *arg)
{
	struct offset_and_name_args *args = arg;
	
	return args->offset == u_offset_from_search_start
		&& 0 == strcmp(NAME_FOR_UNIQTYPE(u), args->name);
}

int __named_a_internal(const void *obj, const void *arg)
{
	const char *test_typestr = (const char *) arg;
	// FIXME: use our recursive subobject search here? HMM -- semantics are non-obvious.
	CHECK_INIT
	DO_QUERY(obj)
	DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
	INIT_SUBOBJ_SEARCH(obj)

	/* The type is incomplete in the client, but not in the program
	 * as a whole. So we still need to look for a matching subobject. */
	unsigned target_offset = (char*) obj - (char*) alloc_start;
	struct offset_and_name_args args = { target_offset, test_typestr };
	_Bool success =  __liballocs_search_subobjects_spanning(alloc_uniqtype,
			target_offset,
			offset_and_name_match_cb,
			&args,
			NULL, NULL);
	if (!success) goto named_a_failed;

named_a_succeeded:
	++__libcrunch_succeeded;
out:
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
		report_failure_if_necessary(__builtin_return_address(0),
			NAMED_A,
			cur_obj_uniqtype,
			NULL,
			alloc_uniqtype,
			"named_a(%p, \"%s\"): allocation was a %s%s%s originating at %p", 
				obj, test_typestr,
				a ? a->name : "(no allocator)",
				(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
				NAME_FOR_UNIQTYPE(alloc_uniqtype),
				alloc_site);
	}
	return 1; // HACK: so that the program will continue
}

int 
__check_args_internal(const void *obj, int nargs, ...)
{
	CHECK_INIT
	DO_QUERY(obj)
	/* No need for subobject stuff or array-precisifying stuff. */
	
	struct uniqtype *fun_uniqtype = alloc_uniqtype;
	assert(fun_uniqtype);
	if (!(UNIQTYPE_IS_SUBPROGRAM_TYPE(fun_uniqtype)))
	{
		/* It's not a function. */
		abort();
	}
	if (!(alloc_start == obj)) abort();
	
	/* Walk the arguments that the function expects. Simultaneously, 
	 * walk our arguments. */
	va_list ap;
	va_start(ap, nargs);
	
	// FIXME: this function screws with the __libcrunch_begun count somehow
	// -- try hello-funptr
	
	_Bool success = 1;
	int i;
	for (i = 0; i < nargs && i < fun_uniqtype->un.subprogram.narg; ++i)
	{
		void *argval = va_arg(ap, void*);
		/* related[0] is the return type */
		struct uniqtype *expected_arg = fun_uniqtype->related[i+MIN(1,fun_uniqtype->un.subprogram.nret)].un.t.ptr;
		/* We only check casts that are to pointer targets types.
		 * How to test this? */
		if (UNIQTYPE_IS_POINTER_TYPE(expected_arg))
		{
			struct uniqtype *expected_arg_pointee_type = UNIQTYPE_POINTEE_TYPE(expected_arg);
			success &= __is_aU(argval, expected_arg_pointee_type);
		}
		if (!success) break;
	}
	if (i == nargs && i < fun_uniqtype->un.subprogram.narg)
	{
		/* This means we exhausted nargs before we got to the end of the array.
		 * In other words, the function takes more arguments than we were passed
		 * for checking, i.e. more arguments than the call site passes. 
		 * Not good! */
		success = 0;
	}
	if (i < nargs && i == fun_uniqtype->un.subprogram.narg)
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
out:
	return 1;
}

int __is_a_function_refining_internal(const void *obj, const void *arg)
{
	const struct uniqtype *test_uniqtype = (const struct uniqtype *) arg;
	
	CHECK_INIT
	DO_QUERY(obj)
	/* No need for array stuff or subobject search -- functions don't nest inside functions. */
	
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
			if (UNIQTYPE_IS_SUBPROGRAM_TYPE(alloc_uniqtype))
			{
				/* If our argument counts don't match, that's bad. */
				if (alloc_uniqtype->un.subprogram.narg == test_uniqtype->un.subprogram.narg)
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
					struct uniqtype *alloc_return_type = alloc_uniqtype->related[0].un.t.ptr;
					struct uniqtype *cast_return_type = test_uniqtype->related[0].un.t.ptr;
					
					/* HACK: a little bit of C-specifity is creeping in here.
					 * FIXME: adjust this to reflect sloppy generic-pointer-pointer matches! 
					      (only if LIBCRUNCH_STRICT_GENERIC_POINTERS not set) */
					//	||  (UNIQTYPE_POINTEE_TYPE((to)) == pointer_to___uniqtype__signed_char)
					#define would_always_succeed(from, to) \
						( \
							!UNIQTYPE_IS_POINTER_TYPE((to)) \
						||  (UNIQTYPE_POINTEE_TYPE((to)) == pointer_to___uniqtype__void) \
						||  (UNIQTYPE_IS_POINTER_TYPE((from)) && \
							({ \
								struct starts_at_target_offset_and_has_type_nocache_args args = { \
									0, \
									(to), \
								}; \
								__liballocs_search_subobjects_spanning( \
										/* u */ (from), \
										/* target_offset_within_u */ 0, \
										/* visit_stop_test */ starts_at_target_offset_and_has_type_nocache, \
										/* arg */ &args, \
										/* out_offset */ NULL, \
										/* out_ctxt */ NULL); \
							})))
						
					/* ARGH. Are these the right way round?  
					 * The "implicit cast" is from the alloc'd return type to the 
					 * cast-to return type. */
					success &= would_always_succeed(alloc_return_type, cast_return_type);
					
					if (success) for (int i_rel = MIN(1, alloc_uniqtype->un.subprogram.nret);
						i_rel < MIN(1, alloc_uniqtype->un.subprogram.nret) + 
								alloc_uniqtype->un.subprogram.narg; ++i_rel)
					{
						/* ARGH. Are these the right way round?  
						 * The "implicit cast" is from the cast-to arg type to the 
						 * alloc'd arg type. */
						success &= would_always_succeed(
							test_uniqtype->related[i_rel].un.t.ptr,
							alloc_uniqtype->related[i_rel].un.t.ptr
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
		report_failure_if_necessary(__builtin_return_address(0),
			IS_A_FUNCTION_REFINING,
			alloc_uniqtype,
			test_uniqtype,
			alloc_uniqtype,
			"(%p, %p a.k.a. \"%s\"): "
				"found an allocation of a %s%s%s "
				"originating at %p", 
			obj, test_uniqtype, NAME_FOR_UNIQTYPE(test_uniqtype),
			a ? a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
			NAME_FOR_UNIQTYPE(alloc_uniqtype),
			alloc_site
		);	
	}
out:
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
		//|| ultimate_pointee_type == pointer_to___uniqtype__signed_char
		//|| ultimate_pointee_type == pointer_to___uniqtype__unsigned_char
		;
}

static _Bool holds_pointer_of_degree(struct uniqtype *u, int d, unsigned target_offset)
{
	/* Descend the subobject hierarchy until we can't go any further (since pointers
	 * are atomic. */
	unsigned distance_traversed = 0;
	struct uniqtype_rel_info *contained_pos = NULL;
	struct uniqtype *deepest = __liballocs_deepest_span(u, target_offset,
			&distance_traversed, NULL);
	if (distance_traversed == target_offset && UNIQTYPE_IS_POINTER_TYPE(deepest))
	{
		_Bool depth_okay = pointer_has_degree(deepest, d);
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

static _Bool is_abstract_pointer_type(struct uniqtype *t)
{
	return UNIQTYPE_IS_POINTER_TYPE(t)
		&& t->make_precise;
}

int __loosely_like_a_internal(const void *obj, const void *arg)
{
	struct uniqtype *test_uniqtype = (struct uniqtype *) arg;
	
	CHECK_INIT
	DO_QUERY(obj)
	DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
	INIT_SUBOBJ_SEARCH(obj)
	/* We don't update these, nor need them in this function, so
	 * null them to avoid surprises. */
	cur_containing_uniqtype = NULL;
	cur_contained_pos = NULL;

	if (__builtin_expect(err != NULL, 0)) return 1; // liballocs has already counted this abort

	/* Descend the subobject hierarchy until our target offset is zero, i.e. we 
	 * find the outermost thing in the subobject tree that starts at the address
	 * we were passed (obj). FIXME: doesn't seem quite right; what if we want
	 * to unwrap one more level? */
	unsigned distance_traversed;
	cur_obj_uniqtype = __liballocs_outermost_subobject_at_offset(cur_obj_uniqtype,
		target_offset_within_uniqtype, &distance_traversed);
	if (!cur_obj_uniqtype) goto loosely_like_a_failed;
	target_offset_within_uniqtype -= distance_traversed;
	assert(target_offset_within_uniqtype == 0);

	do
	{
		
	// trivially, identical types are like one another
	if (test_uniqtype == cur_obj_uniqtype) goto loosely_like_a_succeeded;
	
	// if our check type is a pointer type
	int real_degree;
	if (UNIQTYPE_IS_POINTER_TYPE(test_uniqtype)
			&& 0 != (real_degree = is_generic_pointer_type_of_degree_at_least(test_uniqtype, 1)))
	{
		// the pointed-to object must have at least the same degree
		if (holds_pointer_of_degree(cur_obj_uniqtype, real_degree, 0))
		{
			++__libcrunch_succeeded;
			return 1;
		}
		
		// nothing is like a non-generic pointer
		goto try_deeper;
	}
	
	// arrays are special
	_Bool matches;
	if (__builtin_expect(UNIQTYPE_IS_ARRAY_TYPE(cur_obj_uniqtype)
			|| UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype), 0))
	{
		matches = 
			test_uniqtype == cur_obj_uniqtype
		||  (UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype) && UNIQTYPE_ARRAY_LENGTH(test_uniqtype) == 1 
				&& UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype) == cur_obj_uniqtype)
		||  (UNIQTYPE_IS_ARRAY_TYPE(cur_obj_uniqtype) && UNIQTYPE_ARRAY_LENGTH(cur_obj_uniqtype) == 1
				&& UNIQTYPE_ARRAY_ELEMENT_TYPE(cur_obj_uniqtype) == test_uniqtype);
		/* We don't need to allow an array of one blah to be like a different
		 * array of one blah, because they should be the same type. 
		 * FIXME: there's a difficult case: an array of statically unknown length, 
		 * which happens to have length 1. */
		if (matches) goto loosely_like_a_succeeded; else goto try_deeper;
	}
	
	/* We might have base types with signedness complements. */
	if (__builtin_expect(
			!UNIQTYPE_IS_BASE_TYPE(cur_obj_uniqtype) 
			|| UNIQTYPE_IS_BASE_TYPE(test_uniqtype), 0))
	{
		/* Does the cur obj type have a signedness complement matching the test type? */
		if (UNIQTYPE_BASE_TYPE_SIGNEDNESS_COMPLEMENT(cur_obj_uniqtype)
				== test_uniqtype) goto loosely_like_a_succeeded;
		/* Does the test type have a signedness complement matching the cur obj type? */
		if (UNIQTYPE_BASE_TYPE_SIGNEDNESS_COMPLEMENT(test_uniqtype)
				== cur_obj_uniqtype) goto loosely_like_a_succeeded;
	}
	
	/* Okay, we can start the like-a test: for each element in the test type, 
	 * do we have a type-equivalent in the object type?
	 * 
	 * We make an exception for arrays of char (signed or unsigned): if an
	 * element in the test type is such an array, we skip over any number of
	 * fields in the object type, until we reach the offset of the end element.  */
	unsigned i_obj_subobj = 0, i_test_subobj = 0;
	if (test_uniqtype != cur_obj_uniqtype) debug_println(0, "__loosely_like_a proceeding on subobjects of (test) %s and (object) %s",
		NAME_FOR_UNIQTYPE(test_uniqtype), NAME_FOR_UNIQTYPE(cur_obj_uniqtype));
	for (; 
		i_obj_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype)
			&& i_test_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype); 
		++i_test_subobj, ++i_obj_subobj)
	{
		debug_println(0, "Subobject types are (test) %s and (object) %s",
			NAME_FOR_UNIQTYPE((struct uniqtype *) test_uniqtype->related[i_test_subobj].un.memb.ptr), 
			NAME_FOR_UNIQTYPE((struct uniqtype *) cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr));
		
		if (__builtin_expect(UNIQTYPE_IS_ARRAY_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
			&& (UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
					== pointer_to___uniqtype__signed_char
			|| UNIQTYPE_ARRAY_ELEMENT_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
					== pointer_to___uniqtype__unsigned_char), 0))
		{
			// we will skip this field in the test type
			unsigned target_off =
				UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype) > i_test_subobj + 1
			 ?  test_uniqtype->related[i_test_subobj + 1].un.memb.off
			 :  test_uniqtype->related[i_test_subobj].un.memb.off
			      + test_uniqtype->related[i_test_subobj].un.memb.ptr->pos_maxoff;
			
			// ... if there's more in the test type, advance i_obj_subobj
			while (i_obj_subobj + 1 < UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype) &&
				cur_obj_uniqtype->related[i_obj_subobj + 1].un.memb.off < target_off) ++i_obj_subobj;
			/* We fail if we ran out of stuff in the actual object type
			 * AND there is more to go in the test (cast-to) type. */
			if (i_obj_subobj + 1 >= UNIQTYPE_COMPOSITE_MEMBER_COUNT(cur_obj_uniqtype)
			 && UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype) > i_test_subobj + 1) goto try_deeper;
				
			continue;
		}
		
		int generic_ptr_degree = 0;
		matches = 
				(test_uniqtype->related[i_test_subobj].un.memb.off
				== cur_obj_uniqtype->related[i_obj_subobj].un.memb.off)
		 && (
				// exact match
				(test_uniqtype->related[i_test_subobj].un.memb.ptr
				 == cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr)
				|| // loose match: if the test type has a generic ptr...
				(
					0 != (
						generic_ptr_degree = is_generic_pointer_type_of_degree_at_least(
							test_uniqtype->related[i_test_subobj].un.memb.ptr, 1)
					)
					&& pointer_has_degree(
						(struct uniqtype *) cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr,
						generic_ptr_degree
					)
				)
				|| // loose match: signed/unsigned
				(UNIQTYPE_IS_BASE_TYPE(test_uniqtype->related[i_test_subobj].un.memb.ptr)
				 && UNIQTYPE_IS_BASE_TYPE(cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr)
				 && 
				 (UNIQTYPE_BASE_TYPE_SIGNEDNESS_COMPLEMENT((struct uniqtype *) test_uniqtype->related[i_test_subobj].un.memb.ptr)
					== ((struct uniqtype *) cur_obj_uniqtype->related[i_obj_subobj].un.memb.ptr))
				)
		);
		if (!matches) goto try_deeper;
	}
	// if we terminated because we ran out of fields in the target type, fail
	if (i_test_subobj < UNIQTYPE_COMPOSITE_MEMBER_COUNT(test_uniqtype)) goto try_deeper;
	
	// if we got here, we succeeded
	goto loosely_like_a_succeeded;
	
	_Bool success;
	try_deeper:
		debug_println(1, "No dice; will try the object type one level down if there is one...");
		success = 0;
		struct uniqtype_rel_info *contained_ctxt = __liballocs_find_span(cur_obj_uniqtype,
				target_offset_within_uniqtype, NULL);
		if (contained_ctxt)
		{
			success = 1;
			unsigned distance_traversed = UNIQTYPE_SUBOBJECT_OFFSET(cur_obj_uniqtype,
				contained_ctxt, target_offset_within_uniqtype);
			target_offset_within_uniqtype = target_offset_within_uniqtype - distance_traversed;
			cur_obj_uniqtype = UNIQTYPE_SUBOBJECT_TYPE(cur_obj_uniqtype,
				contained_ctxt);
		}
		if (!success) goto loosely_like_a_failed;
		debug_println(1, "... got %s", NAME_FOR_UNIQTYPE(cur_obj_uniqtype));

	} while (1);
	
loosely_like_a_succeeded:
	if (test_uniqtype != alloc_uniqtype) debug_println(0, "__loosely_like_a succeeded! test type %s, allocation type %s",
		NAME_FOR_UNIQTYPE(test_uniqtype), NAME_FOR_UNIQTYPE(alloc_uniqtype));
	++__libcrunch_succeeded;
out:
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
		report_failure_if_necessary(__builtin_return_address(0),
			LOOSELY_LIKE_A,
			cur_obj_uniqtype,
			test_uniqtype,
			alloc_uniqtype,
			"(%p, %p a.k.a. \"%s\"): allocation was a %s%s%s originating at %p", 
			obj, test_uniqtype, NAME_FOR_UNIQTYPE(test_uniqtype),
			a ? a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
			NAME_FOR_UNIQTYPE(alloc_uniqtype), 
			alloc_site);
	}
	return 1; // HACK: so that the program will continue
}

static _Bool try_specialize_memrange_type(
						struct uniqtype *current_type,
						const void *range_start,
						size_t range_len_bytes,
						struct uniqtype *specialized_type_or_element_type,
						struct allocator *a,
						const void *containing_alloc_start,
						struct uniqtype *containing_alloc_type)
{
	/* We can specialize an existential T, or an array thereof,
	 * to a concrete T of the same size,
	 * or an array thereof. */
	_Bool range_is_whole_alloc = (range_start == containing_alloc_start)
			&& range_len_bytes == containing_alloc_type->pos_maxoff;
	if (!range_is_whole_alloc) return 0;
	if (!a || !a->set_type) return 0;

	_Bool success = 0;
	// array case
	if (UNIQTYPE_IS_ARRAY_TYPE(current_type)
			&& is_abstract_pointer_type(UNIQTYPE_ARRAY_ELEMENT_TYPE(current_type))
			&& UNIQTYPE_IS_POINTER_TYPE(specialized_type_or_element_type))
	{
		unsigned array_len = range_len_bytes /
			specialized_type_or_element_type->pos_maxoff;
		// HACK: use arrays until we have proper memranges
		struct uniqtype *target_type;
		if (array_len != 1) target_type = __liballocs_get_or_create_array_type(specialized_type_or_element_type,
				array_len);
		else target_type = specialized_type_or_element_type;
		a->set_type(NULL, (void *) containing_alloc_start, target_type);
		if (a->is_cacheable)
		{
			cache_is_a((char*) range_start,
				/* range_limit */ (char*) range_start + range_len_bytes,
				/* period */ UNIQTYPE_SIZE_IN_BYTES(target_type),
				/* offset_to_a_t */ 0,
				/* t */ target_type,
				/* depth */ 1 /* HACK FIXME */);
		}
		success = 1;
	}
	// singleton case
	else if (is_abstract_pointer_type(current_type)
			&& UNIQTYPE_IS_POINTER_TYPE(specialized_type_or_element_type))
	{
		a->set_type(NULL, (void *) containing_alloc_start, specialized_type_or_element_type);
		success = 1;
	}

	if (success)
	{
		debug_println(0, "libcrunch: specialised allocation at %p from %s to %s (or array thereof)",
			containing_alloc_start,
			NAME_FOR_UNIQTYPE(current_type),
			NAME_FOR_UNIQTYPE(specialized_type_or_element_type));
		++__libcrunch_succeeded_by_specialization;
	}
	return success;
}

int __is_a_pointer_of_degree_internal(const void *obj, int d)
{
	CHECK_INIT
	DO_QUERY(obj)
	DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
	INIT_SUBOBJ_SEARCH(obj)

	if (holds_pointer_of_degree(alloc_uniqtype, d, target_offset_within_uniqtype))
	{
		++__libcrunch_succeeded;
		return 1;
	} // else goto ...
	
is_a_pointer_failed:
	++__libcrunch_failed;
	report_failure_if_necessary(__builtin_return_address(0),
		IS_A_POINTER_OF_DEGREE,
		cur_obj_uniqtype,
		NULL,
		alloc_uniqtype,
		"(%p, %d): "
			"found an allocation of a %s%s%s "
			"originating at %p", 
		obj, d,
		a ? a->name : "(no allocator)",
		(ALLOC_IS_DYNAMICALLY_SIZED(alloc_start, alloc_site) && alloc_uniqtype && alloc_size_bytes > alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
		NAME_FOR_UNIQTYPE(alloc_uniqtype),
		alloc_site);
out:
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
	unsigned target_offset;
	struct uniqtype *out_seen_at_target_offset;
};
static _Bool match_pointer_subobj_strict_cb(struct uniqtype *u,
	struct uniqtype_containment_ctxt *ucc, unsigned u_offset_from_search_start, void *arg)
{
	/* We're storing a pointer that is legitimately a pointer to t (among others) */
	struct match_cb_args *args = (struct match_cb_args *) arg;
	struct uniqtype *type_we_can_store = UNIQTYPE_POINTEE_TYPE(args->type_of_pointer_being_stored_to);
	
	if (u_offset_from_search_start == args->target_offset)
	{
		args->out_seen_at_target_offset = u;
		if (type_we_can_store == u)
		{
			return 1;
		}
	}
	return 0;
}
static _Bool match_pointer_subobj_generic_cb(struct uniqtype *u,
	struct uniqtype_containment_ctxt *ucc, unsigned u_offset_from_search_start, void *arg)
{
	/* We're storing a pointer that is legitimately a pointer to t (among others) */
	struct match_cb_args *args = (struct match_cb_args *) arg;
	
	int degree_of_pointer_stored_to = pointer_degree(args->type_of_pointer_being_stored_to);
	if (u_offset_from_search_start == args->target_offset)
	{
		args->out_seen_at_target_offset = u;
		/* Storing to a degree-1 generic pointer, we can point to anything
		 * as long as it begins exactly at the pointed-to address.*/
		if((!UNIQTYPE_IS_POINTER_TYPE(u) && degree_of_pointer_stored_to == 1)
			/* ... or, if we're storing to a degree-2 pointer (say), it should
			 * point to a degree-1 (or higher) pointer. */
				|| pointer_has_degree(u, degree_of_pointer_stored_to - 1))
		{
			return 1;
		}
	}
	return 0;
}

/* HACK HACK HACK HACK */
struct uniqtype *get_or_create_type_of_pointer_to(struct uniqtype *t)
{
	const char *n = NAME_FOR_UNIQTYPE(t);
	// now we've got the bit we want to splice
	const char prefix[] = "__uniqtype____PTR_";
	unsigned n_len = strlen(n);
	char buf[(sizeof prefix) - 1 + n_len + 1];
	int ret = snprintf(buf, sizeof buf, "%s%s", prefix, n);
	assert(ret == (sizeof prefix) - 1 + n_len); // snprintf doesn't count the null byte
	void *found_existing = dlsym(NULL, buf);
	if (found_existing) return (struct uniqtype *) found_existing;
	return NULL; // FIXME: dlalloc/dlbind it
}
extern struct uniqtype *pointer_to___uniqtype____PTR___PTR_void;
int __can_hold_pointer_internal(const void *obj, const void *value)
{
	struct allocator *obj_a;
	const void *obj_alloc_start;
	size_t obj_alloc_size_bytes;
	struct uniqtype *obj_alloc_uniqtype;
	const void *obj_alloc_site;
	unsigned obj_target_offset_within_uniqtype;
	struct uniqtype *cur_obj_within_alloc_uniqtype;
	struct uniqtype *obj_cur_containing_uniqtype;
	struct uniqtype_rel_info *obj_cur_contained_pos;
	{
		CHECK_INIT
		DO_QUERY(obj)
		DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
		INIT_SUBOBJ_SEARCH(obj)
		obj_a = a;
		obj_alloc_start = alloc_start;
		obj_alloc_size_bytes = alloc_size_bytes;
		obj_alloc_uniqtype = alloc_uniqtype;
		obj_alloc_site = alloc_site;
		obj_target_offset_within_uniqtype = target_offset_within_uniqtype;
		cur_obj_within_alloc_uniqtype = cur_obj_uniqtype;
		obj_cur_containing_uniqtype = cur_containing_uniqtype;
		obj_cur_contained_pos = cur_contained_pos;
	}
	
	/* Descend the subobject hierarchy until we can't go any further (since pointers
	 * are atomic. */
	unsigned distance_traversed;
	struct uniqtype *type_of_pointer_being_stored_to = __liballocs_deepest_span(
		cur_obj_within_alloc_uniqtype, obj_target_offset_within_uniqtype,
		&distance_traversed, NULL
	);
	
	struct allocator *value_pointee_a = NULL;
	const void *value_pointee_alloc_start = NULL;
	unsigned long value_pointee_alloc_size_bytes = (unsigned long) -1;
	struct uniqtype *value_pointee_alloc_uniqtype = (struct uniqtype *)0;
	const void *value_pointee_alloc_site = NULL;
	unsigned value_pointee_target_offset_within_uniqtype = 0;
	_Bool value_pointee_contract_is_specialisable = 0;
	
	/* Might we be storing to pointer? */
	_Bool success = 0;
	int d;
	struct uniqtype *ultimate_pointee_type;
	_Bool is_generic;
	// we're not storing to a pointer, so not a lot we can do
	// (FIXME: until we can specialize a raw-bytes or 'a allocation into pointers...)
	if (distance_traversed != obj_target_offset_within_uniqtype
			|| !UNIQTYPE_IS_POINTER_TYPE(type_of_pointer_being_stored_to))
		goto can_hold_pointer_failed;

	pointer_degree_and_ultimate_pointee_type(type_of_pointer_being_stored_to, &d, &ultimate_pointee_type);
	assert(d > 0);
	assert(ultimate_pointee_type);

	/* Is this a generic pointer, of zero degree? */
	is_generic = is_generic_ultimate_pointee(ultimate_pointee_type);
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
	{
		DO_QUERY(value)
		DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
		INIT_SUBOBJ_SEARCH(value)
		value_pointee_a = a;
		value_pointee_alloc_start = alloc_start;
		value_pointee_alloc_size_bytes = alloc_size_bytes;
		value_pointee_alloc_uniqtype = alloc_uniqtype;
		value_pointee_alloc_site = alloc_site;

		value_pointee_target_offset_within_uniqtype = target_offset_within_uniqtype;
	}
#if 0
	// cache what we know about the pointed-to alloc
	// HMM -- this seems not to help
	if (value_pointee_a->is_cacheable)
	{
		cache_is_a((char*) value_pointee_alloc_start,
			/* range_limit */ (char*) value_pointee_alloc_start + value_pointee_alloc_size_bytes,
			/* period */
				UNIQTYPE_IS_ARRAY_TYPE(value_pointee_alloc_uniqtype) ?
					UNIQTYPE_SIZE_IN_BYTES(UNIQTYPE_ARRAY_ELEMENT_TYPE(value_pointee_alloc_uniqtype))
					: UNIQTYPE_SIZE_IN_BYTES(value_pointee_alloc_uniqtype),
			/* offset_to_a_t */ 0,
			/* t */ UNIQTYPE_IS_ARRAY_TYPE(value_pointee_alloc_uniqtype) ?
					UNIQTYPE_ARRAY_ELEMENT_TYPE(value_pointee_alloc_uniqtype)
					: value_pointee_alloc_uniqtype,
			/* depth */ 1 /* HACK FIXME */);
	}
#endif

	/* See if the top-level pointee object matches */
	struct match_cb_args args = {
		.type_of_pointer_being_stored_to = type_of_pointer_being_stored_to,
		.target_offset = value_pointee_target_offset_within_uniqtype,
		.out_seen_at_target_offset = NULL
	};
	/* Here we walk the pointed-at subobject hierarchy until we hit
	 * one that is at the offset the pointer value actually points at
	 * and equals the stored-to pointer's target type. Or if the stored-to
	 * pointer is generic, it doesn't have to match exactly but should
	 * be a degreewise match (i.e. degree of no more than one less). */
	if (!success) success = __liballocs_search_subobjects_spanning(
		value_pointee_alloc_uniqtype,
		value_pointee_target_offset_within_uniqtype,
		is_generic ? match_pointer_subobj_generic_cb : match_pointer_subobj_strict_cb,
		&args,
		NULL, NULL);

	if (success)
	{
		++__libcrunch_succeeded;
		return 1;
	}
	assert(!success);
	/* If we got here, it looks like we can't store this pointer there,
	 * because it points to an allocation of type T and that's not compatible
	 * with the type of the written-to storage S.
	 */
	/* BUT WAIT. Two cases: S is abstract, or T is abstract. We can specialize
	 * in either of them. */
	/* First case: S is a pointer-to-abstract. */
	if (/*is_abstract_pointer_type(type_of_pointer_being_stored_to)
			&& */ UNIQTYPE_IS_ARRAY_TYPE(obj_alloc_uniqtype)
			&& UNIQTYPE_ARRAY_ELEMENT_TYPE(obj_alloc_uniqtype) == &__uniqtype____EXISTS1___PTR__1
			&& args.out_seen_at_target_offset)
	{
		/* We can just specialize it and continue, right?
		 * "Real" example, from milc:
		 *       dest[j] = ((char *)(lattice+neighbor[index][j]))+field;
		 * where "dest" has type char**
		 * and *dest is an array of __uniqtype____EXISTS1___PTR__1
		 * and the pointer on the rhs really points into an array of 2000 __anonstruct_site_1058508169s
		 * ... but not necessarily to the beginning of one of those!
		 * How do we know we want the specific seen-at-target type we want to specialise to?
		 * We have many target types to choose from: void, or anything that
		 * begins at the pointed-at offset.
		 * Maybe we could leave it existential? No, because then there is a
		 * uniformity property that we can't enforce: that 'a is the same
		 * across the entire array (if there is an array).
		 * Also we assume below that any pointer-to-'a "has not yet been written to".
		 *
		 * So when we write to it, we had better decide a type for it. Safest is
		 * void*. But if we fix that, we may later fail if we want to cast the
		 * array's base address to int**, say. Under the discipline we have chosen,
		 * such a cast needs to come before any write. In the current case, we are
		 * witnessing the opposite ordering (write first), so yield an array of void*.
		 */
		success = try_specialize_memrange_type(
			obj_alloc_uniqtype,
			obj_alloc_start,
			obj_alloc_size_bytes,
			/* "pointer to value pointee type"! how do we find this? */
			//get_or_create_type_of_pointer_to(args.out_seen_at_target_offset),
			pointer_to___uniqtype____PTR_void,
			obj_a,
			obj_alloc_start,
			obj_alloc_uniqtype
			);
		if (success) { ++__libcrunch_succeeded_by_specialization; return 1; }
	}
	/* A bit more head-twistingly: what if it's not compatible because T is abstract?
	 * We can specialize T and carry on.
	 * By construction, if allocation T has abstract type then
	 * no pointer within it has yet been written to, nor has its address
	 * (or the address of any such contained pointer or pointer-containing subobject)
	 * been cast to a pointer to a non-abstract type. */
	else if (!is_abstract_pointer_type(type_of_pointer_being_stored_to)
		&& value_pointee_alloc_uniqtype
		&& is_abstract_pointer_type(value_pointee_alloc_uniqtype)
		&& 0 == value_pointee_target_offset_within_uniqtype % UNIQTYPE_SIZE_IN_BYTES(value_pointee_alloc_uniqtype)
	)
	{
		success = try_specialize_memrange_type(
			value_pointee_alloc_uniqtype,
			value_pointee_alloc_start,
			value_pointee_alloc_size_bytes,
			type_of_pointer_being_stored_to,
			value_pointee_a,
			value_pointee_alloc_start,
			value_pointee_alloc_uniqtype
			);
		if (success) { ++__libcrunch_succeeded_by_specialization; return 1; }
	}

can_hold_pointer_failed:
	assert(!success);
	if (__currently_allocating || __currently_freeing)
	{
		++__libcrunch_failed_in_alloc;
		// suppress warning
	}
	else
	{
		++__libcrunch_failed;
		report_failure_if_necessary(__builtin_return_address(0),
			CAN_HOLD_POINTER,
			NULL,
			NULL,
			NULL,
			"(%p, %p): "
				"target pointer is a %s, %ld bytes into a %s%s%s originating at %p, "
				"value points %ld bytes into a %s%s%s originating at %p", 
			obj, value,
			NAME_FOR_UNIQTYPE(type_of_pointer_being_stored_to),
			(long)((char*) obj - (char*) obj_alloc_start),
			obj_a ? obj_a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(obj_alloc_start, obj_alloc_site) && obj_alloc_uniqtype && obj_alloc_size_bytes > obj_alloc_uniqtype->pos_maxoff) ? " allocation of " : " ",
			NAME_FOR_UNIQTYPE(obj_alloc_uniqtype),
			obj_alloc_site,
			(long)((char*) value - (char*) value_pointee_alloc_start),
			value_pointee_a ? value_pointee_a->name : "(no allocator)",
			(ALLOC_IS_DYNAMICALLY_SIZED(value_pointee_alloc_start, value_pointee_alloc_site) && value_pointee_alloc_uniqtype && value_pointee_alloc_size_bytes > value_pointee_alloc_uniqtype->pos_maxoff) ? " allocation of " : " ", 
			NAME_FOR_UNIQTYPE(value_pointee_alloc_uniqtype),
			value_pointee_alloc_site
			);
	}
out:
	return 1; // fail, but program continues
}
extern void *__real___notify_copy(void *dest, const void *src, size_t n);
void **__libcrunch_ool_base_stored_addr(void *const *stored_ptr_addr);
unsigned *__libcrunch_ool_size_stored_addr(void *const *stored_ptr_addr);
void *__wrap___notify_copy(void *dest, const void *src, size_t n)
{
	/* Do nothing if the shadow space is not initialized. */
	/* WEIRD. If I replace "goto out" with "return dest" here, 
	 * gcc 4.9.2 miscompiles this by returning 0 (xor eax,eax).
	 * HACK around this with an "out" label for now. */
	if (!__libcrunch_bounds_bases_region_00) goto out;
	
	// HACK: a bit stronger -- WHY? this breaks Softbound emulation unless we do the init here
	// if (!__libcrunch_is_initialized) goto out;
	/* A realloc, memcpy, memmove or similar operation is moving some data. 
	 * Make sure we
	 * -- move its metadata;
	 * -- FIXME: check the type-correctness of the copy.
	 */
	if (!orig_memmove)
	{
		/* NOTE: memmove is an IFUNC symbol on glibc.
		 * So fake_dlsym is invoking the ifunc for us. */
		orig_memmove = fake_dlsym(RTLD_NEXT, "memmove");
	}
	orig_memmove(__libcrunch_ool_base_stored_addr(dest), 
		__libcrunch_ool_base_stored_addr(src),
		n);
	
	orig_memmove(__libcrunch_ool_size_stored_addr(dest), 
		__libcrunch_ool_size_stored_addr(src),
		n * sizeof (unsigned) / sizeof (void*));
out:
	return __real___notify_copy(dest, src, n);
}

static void cache_containment_facts(struct uniqtype *ultimate_u,
	struct uniqtype *contained_u,
	struct uniqtype_containment_ctxt *ucc,
	unsigned distance_walked_back)
{
	// This is called from bounds_cb when we succeed at locating
	// TODO: fill me in once we know what we want to cache.
}

struct bounds_cb_arg
{
	struct uniqtype *passed_in_t;
	unsigned target_offset;
	_Bool success;
	struct uniqtype *matched_t;
	struct uniqtype *innermost_containing_array_t;
	unsigned innermost_containing_array_type_span_start_offset;
	struct uniqtype *outermost_containing_array_t;
	unsigned outermost_containing_array_type_span_start_offset;
	size_t accum_array_bounds;
};

static _Bool bounds_cb(struct uniqtype *u, struct uniqtype_containment_ctxt *ucc,
	unsigned u_offset_from_search_start, void *arg_void)
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
	if (ucc && ucc->u_container && UNIQTYPE_IS_ARRAY_TYPE(ucc->u_container))
	{
		arg->innermost_containing_array_type_span_start_offset
		 = u_offset_from_search_start - ucc->u_offset_within_container;
		arg->innermost_containing_array_t = ucc->u_container;
		// FIXME: get rid of innermost and outermost and just walk the contexts
		if (!arg->outermost_containing_array_t)
		{
			arg->outermost_containing_array_type_span_start_offset
			 = u_offset_from_search_start - ucc->u_offset_within_container;
			arg->outermost_containing_array_t = ucc->u_container;
			arg->accum_array_bounds = UNIQTYPE_ARRAY_LENGTH(ucc->u_container);
			if (arg->accum_array_bounds < 1) arg->accum_array_bounds = 0;
		}
		else arg->accum_array_bounds *= UNIQTYPE_ARRAY_LENGTH(ucc->u_container);
	}
	else
	{
		arg->outermost_containing_array_type_span_start_offset = 0;
		arg->outermost_containing_array_t = NULL;
		arg->innermost_containing_array_type_span_start_offset = 0;
		arg->innermost_containing_array_t = NULL;
		arg->accum_array_bounds = 0;
	}
	
	if (u_offset_from_search_start < arg->target_offset)
	{
		return 0; // keep going
	}
	
	// now we have span_start_offset >= target_offset
	if (u_offset_from_search_start > arg->target_offset)
	{
		/* We've overshot. If this happens, it means the target offset
		 * is not a subobject start offset. This shouldn't happen,
		 * unless the caller makes a wild pointer. */
		return 1;
	}
	
	if (u_offset_from_search_start == arg->target_offset)
	{
		/* We've hit a subobject that starts at the right place.
		 * It might still be an enclosing object, not the object we're
		 * looking for. We differentiate using the size of the passed-in
		 * type -- this is the size of object that the pointer
		 * arithmetic is being done on. Keep going til we hit something
		 * of that size. */
		if (u->pos_maxoff < arg->passed_in_t->pos_maxoff)
		{
			// usually shouldn't happen, but might with __like_a prefixing
			// arg->success = 1;
			// return 1;
			// EXCEPT it also happens with stack frames that are de-facto unions.
			// In that case, we should continue with the sibling members,
			// looking for one whose size does match.
			return 0; // keep going here too
		}
		if (u->pos_maxoff > arg->passed_in_t->pos_maxoff)
		{
			// keep going
			return 0;
		}
		
		assert(u->pos_maxoff == arg->passed_in_t->pos_maxoff);
		arg->success = 1;
		arg->matched_t = u;
		cache_containment_facts(u, ucc);

		return 1;
	}
	
	assert(0);
	__builtin_unreachable();
}

__libcrunch_bounds_t __fetch_bounds_internal(const void *obj, const void *derived, struct uniqtype *t)
{
	if (!obj) goto return_min_bounds;
	
	CHECK_INIT
	DO_QUERY(obj)
	DO_PRECISIFY(alloc_uniqtype, alloc_start, alloc_size_bytes, a)
	INIT_SUBOBJ_SEARCH(obj)

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
		goto return_alloc_bounds; // FIXME: this is C-specific -- belongs in front-end instrumentation (__fetch_alloc_bounds?)
	}
	if (__builtin_expect( t->pos_maxoff == UNIQTYPE_POS_MAXOFF_UNBOUNDED, 0))
	{
		goto return_min_bounds; // FIXME: also belongs in instrumentation -- can test for an incomplete type
	}	                        // -- bounds are no use if the caller thinks it's incomplete, even if does have bounded size

	/* For bounds checking, 
	 * what we're really asking about is the regularity of the memory around obj,
	 * when considered in strides of t->pos_maxoff. 
	 * It doesn't actually matter what t is.
	 * So:
	 * 
	 * - find the outermost uniqtype at offset obj - alloc_start
	 * - descend (offset zero) until we find something of *the same size or smaller than* 
	          t->pos_maxoff
	 * - if we find smaller, that means we used __like_a prefixing; bounds are only the pointee;
	 * - if we find equi-sized, use the bounds of the containing array if there is one.
	 */

	struct bounds_cb_arg arg = {
		.passed_in_t = t,
		.target_offset = target_offset_within_uniqtype
	};
	_Bool ret = __liballocs_search_subobjects_spanning(
		alloc_uniqtype, 
		target_offset_within_uniqtype,
		bounds_cb, 
		&arg,
		NULL, NULL
	);
	if (arg.success)
	{
		if (arg.innermost_containing_array_t)
		{
			// bounds are the whole array
			const char *lower = (char*) alloc_start + arg.innermost_containing_array_type_span_start_offset;
			const char *upper = (UNIQTYPE_ARRAY_LENGTH(arg.innermost_containing_array_t) == 0) ? /* use the allocation's limit */ 
					alloc_start + alloc_size_bytes
					: (char*) alloc_start + arg.innermost_containing_array_type_span_start_offset
						+ (UNIQTYPE_ARRAY_LENGTH(arg.innermost_containing_array_t) * t->pos_maxoff);
			unsigned period = UNIQTYPE_ARRAY_ELEMENT_TYPE(arg.innermost_containing_array_t)->pos_maxoff;
			if (a && a->is_cacheable) cache_bounds(lower, upper, period, 0, t,
				/* depth HACK FIXME */ arg.innermost_containing_array_type_span_start_offset == 0 ? 0 : 1);
			return __make_bounds(
				(unsigned long) lower,
				(unsigned long) upper
			);
		}
		// bounds are just this object
		char *limit = (char*) obj + (t->pos_maxoff > 0 ? t->pos_maxoff : 1);
		if (a && a->is_cacheable)
		{
			cache_bounds(obj, limit, (t->pos_maxoff > 0 ? t->pos_maxoff : 1), 0, t, /* HACK FIXME depth */ 1);
		}
		return __make_bounds((unsigned long) obj, (unsigned long) limit);
	}
	else
	{
		debug_println(1, "libcrunch: no bounds for %p, target type %s, offset %d in allocation of %s at %p", 
			obj, NAME_FOR_UNIQTYPE(t), target_offset_within_uniqtype, NAME_FOR_UNIQTYPE(alloc_uniqtype),
			alloc_start);
		goto return_min_bounds;
	}

return_min_bounds:
	if (a && a->is_cacheable) cache_bounds(obj, (char*) obj + 1, 0, 0, NULL, 1);
	return __make_bounds((unsigned long) obj, (unsigned long) obj + 1);

return_alloc_bounds:
	{
		char *base = (char*) alloc_start;
		char *limit = (char*) alloc_start + alloc_size_bytes;
		unsigned long size = limit - base;
		
		/* CHECK: do the bounds include the derived-from pointer? If not, we abort. */
		if ((unsigned long) obj - (unsigned long) base > size) goto abort_returning_max_bounds;

		// FIXME: the period here seems not right
		// -- if we are chars, should ignore alloc_uniqtype and go for 1?
		if (a && a->is_cacheable) cache_bounds(base,
			limit,
			/* period */ alloc_uniqtype ? alloc_uniqtype->pos_maxoff : 1,
			/* offset to a t, i.e. to a char */ 0,
			t,
			/* HACK FIXME depth */ 0);
		return __make_bounds(
			(unsigned long) base,
			(unsigned long) limit
		);
	}

out:
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
		/* period */ (t ? t->pos_maxoff : 1),
		0,
		t,
		0 /* depth */
	);
	
	debug_println(1, "libcrunch: failed to fetch bounds for pointer %p (deriving %p); liballocs said %s (alloc site %p)", 
			obj, derived, err ? __liballocs_errstring(err) : "no allocation found spanning queried pointer", alloc_site);
	return __libcrunch_max_bounds(obj);
}

#if 0 /* I believe this is dead... */
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
				debug_println(1, "libcrunch: allowing derivation of %p (from %p) owing to cached fake bounds.", derived, derivedfrom);
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
#endif /* end believed dead */
void __libcrunch_bounds_error_at(const void *derived, const void *derivedfrom, 
		__libcrunch_bounds_t bounds, const void *addr)
{
	__libcrunch_check_init();
	
	report_failure_if_necessary(addr,
		DERIVED_PTR_VALID,
		NULL,
		NULL,
		NULL,
		"(derived %p from %p): difference %ld; lb %p; ub %p",
		derived, derivedfrom, 
		(long)((char*) derived - (char*) derivedfrom),
		(void*)__libcrunch_get_base(bounds, derivedfrom), 
		(void*)__libcrunch_get_limit(bounds, derivedfrom));
	++__libcrunch_created_invalid_pointer;
	
	if (!(derivedfrom - __libcrunch_get_base(bounds, derivedfrom)
		 <= __libcrunch_get_size(bounds, derivedfrom)))
	{
		warnx("*** something fishy: derived-from pointer was not in bounds");
	}
}

void __libcrunch_soft_deref_error_at(const void *ptr, __libcrunch_bounds_t bounds, const void *addr)
{
	__libcrunch_check_init();
	
	report_failure_if_necessary(addr,
		DEREFED_PTR_VALID,
		NULL,
		NULL,
		NULL,
		"(%p): lb %p; ub %p",
		ptr,
		(void*)__libcrunch_get_base(bounds, ptr), 
		(void*)__libcrunch_get_limit(bounds, ptr)
	);
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
	/* If we have one-past pointers, pretend we're asking for one before. */
	if (__libcrunch_ptr_trap_bits(ptr) == LIBCRUNCH_TRAP_ONE_PAST)
	{
		ptr = (void*) __libcrunch_detrap((char*) ptr - t->pos_maxoff);
	}
	if (__libcrunch_ptr_trap_bits(derived_ptr) == LIBCRUNCH_TRAP_ONE_PAST)
	{
		derived_ptr = (void*) __libcrunch_detrap((char*) derived_ptr - t->pos_maxoff);
	}
	__libcrunch_bounds_t from_cache = __fetch_bounds_from_cache(
			ptr, derived_ptr, t, t->pos_maxoff
	);
	if (!__libcrunch_bounds_invalid(from_cache, ptr)) return from_cache;
	++__libcrunch_fetch_bounds_missed_cache; /* This should hardly ever happen!
	 * Only with uninstrumented code, or casts not on a recently malloc'd heap object,
	 * or fetching char* bounds, or GPP bounds. */
	return __fetch_bounds_internal(ptr, derived_ptr, t);
}

/* Use this naive libdl-based version */
__libcrunch_bounds_t 
(__attribute__((pure,__const__)) __fetch_bounds_ool_via_dladdr)
(const void *ptr, const void *derived_ptr, struct uniqtype *t)
{
	if (!ptr) return __make_bounds(0, 1);
	Dl_info i = dladdr_with_cache(ptr);
	if (i.dli_fname && i.dli_sname)
	{
		void *obj_handle = get_link_map(i.dli_saddr);
		if (obj_handle)
		{
			Elf64_Sym *found = symbol_lookup_in_object(obj_handle, i.dli_sname);
			if (found)
			{
				void *base = sym_to_addr(found);
				void *limit = (char*) base + found->st_size;
				return __make_bounds((unsigned long) base, (unsigned long) limit);
			}
		}
	}
	warnx("Failed to dladdr-fetch bounds for %p", ptr);
	return __libcrunch_make_invalid_bounds(ptr);
}

void (__attribute__((nonnull(1))) __store_pointer_nonlocal_via_voidptrptr)(const void **dest, const void *srcval, __libcrunch_bounds_t val_bounds, struct uniqtype *static_guessed_srcval_pointee_type);
void (__attribute__((nonnull(1))) __store_pointer_nonlocal_via_voidptrptr)(const void **dest, const void *srcval, __libcrunch_bounds_t val_bounds, struct uniqtype *static_guessed_srcval_pointee_type)
{
#ifdef LIBCRUNCH_KEEP_EXPENSIVE_COUNTS
	++__libcrunch_ptr_stores;
#endif
	/* This is like __store_pointer_nonlocal but the lvalue we're writing through has void* type.
	 * To accommodate polymorphic code, it gets complicated.
	 * We want to make a *fast* guess about the actual pointee type of the target storage.
	 * If it's non-void*, write some non-invalid bounds
	 * The calling instrumentation may have given us a guess of the source value's real
	 * pointer type.
	 * It's not our job to check their compatibility (trumptr does that).
	 * 
	 * NOTE: cache interactions with trumptr get tricky here. We want trumptr's 
	 * write-checking to go first, so that more stuff is in the cache.
	 * Recall that trumptr's instrumentation pass happens *after* crunchbound,
	 * so its checks imdeed do end up immediately after the write instruction.
	 * That's good for us, because we can profit from their effects on the cache. */
#ifndef LIBCRUNCH_NO_SHADOW_SPACE
	unsigned long size_stored_addr = (unsigned long) SIZE_STORED(dest);

	/* WHEE. The caller probably passed us the bounds of "dest" on the shadow stack.
	 * Is this useful? We care about whether the target alloc was allocated as
	 * holding void* or some other pointer type. If it's some other, we might want
	 * to store bounds there. What bounds? The caller didn't pass us the bounds for
	 * the pointer itself (it's void*). Oh, but we have "val_bounds" so maybe it did.
	 * If it was allocated as void*, it's probably harmless to write them. 
	 * So perhaps we should just always write them, if we're going to XOR-check
	 * at shadow-load time?
	 * 
	 * + in our example code, we just loaded the void* value being written.
	 * So we need some way to see the bounds for *it*.
	 
	 * "If loading a void*, always load bounds anyway"? HMM. Breaks stuff. 
	 * I think rooting in the cache is best. Delete the noquery test case. */
	__libcrunch_bounds_t dest_alloc_ptrwise_bounds = __peek_argument_bounds(
		/* really */ 1, /* offset */ 0, /* val */ srcval, "fake peek in " __FILE__);

	struct uniqtype *toplevel_cached_target_alloc_type = __liballocs_get_cached_object_type(dest);
	struct uniqtype *cached_target_alloc_type = toplevel_cached_target_alloc_type;
	if (cached_target_alloc_type)
	{
		/* descend containment until we get a pointer. */
		_Bool success = 1;
		unsigned target_offset = 0;
		struct uniqtype *cur_containing_uniqtype = NULL;
		struct uniqtype_rel_info *cur_contained_pos = NULL;
		unsigned distance_traversed = 0;
		struct uniqtype *deepest = __liballocs_deepest_span(
				cached_target_alloc_type, target_offset, &distance_traversed, NULL);
		if (distance_traversed != target_offset ||
				!UNIQTYPE_IS_POINTER_TYPE(deepest)) cached_target_alloc_type = NULL;
		else cached_target_alloc_type = deepest;
	}

	if (cached_target_alloc_type)
	{
		assert(UNIQTYPE_IS_POINTER_TYPE(cached_target_alloc_type));
		if (!static_guessed_srcval_pointee_type)
		{
			/* Okay, try the cache for that too. We don't officially need the type
			 * of the srcval, but for now I'm more comfortable if we check it agrees. */
			static_guessed_srcval_pointee_type = __liballocs_get_cached_object_type(srcval);
		}
		if (!static_guessed_srcval_pointee_type
			/* If it really is a void* object, we can go ahead  */
			|| UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type) == pointer_to___uniqtype__void
			/* Ditto for any other generic pointer? FIXME: think more about this.  */
			//|| UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type) == pointer_to___uniqtype__signed_char
			//|| UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type) == pointer_to___uniqtype__unsigned_char
			/* ... and we do the same if the static guess agrees with the cache */
			|| UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type) == static_guessed_srcval_pointee_type)
		{
			/* Okay, go with the bounds the caller gave us. If they're invalid, the usual
			 *  __store_pointer_nonlocal thing wil try fetching them, using the type. */
			__store_pointer_nonlocal(dest, srcval, val_bounds, UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type));
		}
		else // static_guessed_srcval_pointee_type &&
			// UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type) != static_guessed_srcval_pointee_type
		{
			/* That's a pity. Report it.
			 * FIXME: what about what the cache says about the pointee value */
			warnx("void** bounds store: disagreed about types: cache says %s (toplevel: %s) but static guess is %s",
				NAME_FOR_UNIQTYPE(UNIQTYPE_POINTEE_TYPE(cached_target_alloc_type)),
				NAME_FOR_UNIQTYPE(toplevel_cached_target_alloc_type),
				NAME_FOR_UNIQTYPE(static_guessed_srcval_pointee_type));
			abort();
		}
	}
	else
	{
		/* The cache didn't know what we're writing to. Write invalid bounds for now. 
		 * FIXME: we may find that doing the slow thing is actually faster. */
		__shadow_store_bounds_for((void**) dest, __libcrunch_make_invalid_bounds(srcval),
			(void*)0);
	}
#endif
}

/* HACK since shadow.o is in libcrunch_stubs not libcrunch_preload.so, but we don't want
 * to link the preload lib against the stubs lib (why not?), provide a dummy
 * definition here to avoid link errors. These are both global, so either this one or
 * the stubs lib one will "win" and will be the only one used. It'll be ours because
 * we're preloaded. */
/* HACK about volatile: see libcrunch_cil_inlines.h (and softbound-heap test case). */
__thread unsigned long *volatile __shadow_sp __attribute__((weak));
extern __thread unsigned long *volatile __bounds_sp __attribute__((weak,alias("__shadow_sp")));

/* Provide out-of-line versions of all the (useful) inlines in libcrunch_cil_inlines.h. */

void __libcrunch_ool_check_cache_sanity(struct __liballocs_memrange_cache *cache)
{
	__liballocs_check_cache_sanity(cache);
}

struct __liballocs_memrange_cache_entry_s *__libcrunch_ool_cache_lookup(struct __liballocs_memrange_cache *cache, const void *obj, struct uniqtype *t, unsigned long require_period)
{
	return __liballocs_memrange_cache_lookup(cache, obj, t, require_period);
}

struct uniqtype * __libcrunch__ool_get_cached_object_type(const void *addr)
{
	return __liballocs_get_cached_object_type(addr);
}

void *__libcrunch_ool_trap(const void *ptr, unsigned short tag)
{
	return __libcrunch_trap(ptr, tag);
}

unsigned long __libcrunch_ool_detrap(const void *any_ptr)
{
	return __libcrunch_detrap(any_ptr);
}

void *__libcrunch_ool_untrap(const void *trapptr, unsigned short tag __attribute__((unused)))
{
	return __libcrunch_untrap(trapptr, tag);
}

unsigned long __libcrunch_ool_ptr_trap_bits(const void *maybe_trap/*, unsigned short tag*/)
{
	return __libcrunch_ptr_trap_bits(maybe_trap);
}

int __libcrunch_ool_is_trap_ptr(const void *maybe_trap/*, unsigned short tag*/)
{
	return __libcrunch_is_trap_ptr(maybe_trap);
}

__libcrunch_bounds_t __libcrunch_ool_make_bounds(unsigned long base, unsigned long limit)
{
	return __make_bounds(base, limit);
}

__libcrunch_bounds_t __libcrunch_ool_max_bounds(const void *ptr)
{
	return __libcrunch_max_bounds(ptr);
}

void * __libcrunch_ool_get_base(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
	return __libcrunch_get_base(bounds, derivedfrom);
}

unsigned long __libcrunch_ool_get_size(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
	return __libcrunch_get_size(bounds, derivedfrom);
}

void *__libcrunch_ool_get_limit(__libcrunch_bounds_t bounds, const void *derivedfrom)
{
	return __libcrunch_get_limit(bounds, derivedfrom);
}

__libcrunch_bounds_t __libcrunch_ool_make_invalid_bounds(const void *ptr)
{
	return __libcrunch_make_invalid_bounds(ptr);
}

_Bool __libcrunch_ool_bounds_invalid(__libcrunch_bounds_t bounds, const void *ptr)
{
	return __libcrunch_bounds_invalid(bounds, ptr);
}

_Bool __libcrunch_ool_valid_bounds_equal(__libcrunch_bounds_t bounds1, __libcrunch_bounds_t bounds2)
{
	return __libcrunch_valid_bounds_equal(bounds1, bounds2);
}

__libcrunch_bounds_t __libcrunch_ool_fetch_bounds_from_cache(const void *ptr, const void *derived_ptr_maybetrapped, struct uniqtype *t, unsigned long t_sz)
{
	return __fetch_bounds_from_cache(ptr, derived_ptr_maybetrapped, t, t_sz);
}

_Bool __libcrunch_ool_secondary_check_derive_ptr(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *p_derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz)
{
	return __secondary_check_derive_ptr(p_derived, derivedfrom, p_derivedfrom_bounds, t, t_sz);
}

_Bool __libcrunch_ool_full_check_derive_ptr(const void **p_derived, const void *derivedfrom, /* __libcrunch_bounds_t *opt_derived_bounds, */ __libcrunch_bounds_t *derivedfrom_bounds, struct uniqtype *t, unsigned long t_sz)
{
	return __full_check_derive_ptr(p_derived, derivedfrom, derivedfrom_bounds, t, t_sz);
}

void __libcrunch_ool_shadow_store_bounds_for(void **stored_pointer_addr, __libcrunch_bounds_t val_bounds, struct uniqtype *t)
{
	__shadow_store_bounds_for(stored_pointer_addr, val_bounds, t);
}

void __libcrunch_ool_store_pointer_nonlocal(const void **dest, const void *val, __libcrunch_bounds_t val_bounds, struct uniqtype *val_pointee_type)
{
	return __store_pointer_nonlocal(dest, val, val_bounds, val_pointee_type);
}

void **__libcrunch_ool_base_stored_addr(void *const *stored_ptr_addr)
{
	return BASE_STORED((void**) stored_ptr_addr);
}

unsigned *__libcrunch_ool_size_stored_addr(void *const *stored_ptr_addr)
{
	return (unsigned *) SIZE_STORED((void**) stored_ptr_addr);
}

__libcrunch_bounds_t __libcrunch_ool_fetch_bounds_from_shadow_space(const void *ptr, void **loaded_from)
{
	return __fetch_bounds_from_shadow_space(ptr, loaded_from);
}

__libcrunch_bounds_t __libcrunch_ool_fetch_bounds_inl(const void *ptr, void **loaded_from, struct uniqtype *t)
{
	return __fetch_bounds_inl(ptr, loaded_from, t);
}

__libcrunch_bounds_t __libcrunch_ool_fetch_bounds_full(const void *ptr, const void *derived, void **loaded_from, struct uniqtype *t)
{
	return __fetch_bounds_full(ptr, derived, loaded_from, t);
}

__libcrunch_bounds_t __libcrunch_ool_peek_argument_bounds(unsigned long offset, const void *ptr)
{
	return __peek_argument_bounds(1, offset, ptr, "ool peek in " __FILE__);
}

__libcrunch_bounds_t __libcrunch_ool_peek_result_bounds(unsigned long offset, const void *ptr)
{
	return __peek_result_bounds(1, offset, ptr, "ool peek in " __FILE__);
}
