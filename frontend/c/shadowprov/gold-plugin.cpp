/* Gold plugin for liballocs.
 * 
 * Copyright 2017, Stephen Kell <stephen.kell@cl.cam.ac.uk>
 * 
 * This duplicates the logic of allocscompilerwrapper.py,
 * but should ultimately be faster.
 * 
 * Since we only run at link time, not after compilation, we
 * assume that input .o files have not yet undergone any of the
 * usual post-compile/assembly fixups (link-used-types etc.).
 */

#include <vector>
#include <string>
#include <regex>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <libgen.h> /* for dirname() and GNU basename() -- must include before cstring */
#include <cstring>
#include <sys/mman.h>
#include <elf.h>
#include <cassert>
#include "plugin-api.h"
#define RELF_DEFINE_STRUCTURES 1
#include "relf.h" /* to get our own binary's realpath -- bit of a HACK */
#include <unistd.h> /* for sleep(), write(), read() */
#include <sys/types.h> /* for fstat() */
#include <sys/stat.h> /* for fstat() */
#include <utility> /* for pair */
//#include <experimental/optional>
#include <boost/optional.hpp>

using std::vector;
using std::string;
using std::smatch;
using std::regex;
using std::regex_match;
using std::pair;
using std::make_pair;
// using std::experimental::optional;
using boost::optional;

/* These will be initialized from the transfer vector. */
static int output_file_type = -1;
static int api_version;
static int gold_version;
static int gnu_ld_version;

static const char *get_exe_basename()
{
	return "(linker plugin)";
}

static int debug_level;
static FILE *stream_err = stderr;
#define debug_printf(lvl, fmt, ...) do { \
    if ((lvl) <= debug_level) { \
      fprintf(stream_err, "%s: " fmt, get_exe_basename(), ## __VA_ARGS__ );  \
    } \
  } while (0)


static
string
get_plugin_path()
{
	return get_highest_loaded_object_below(
		reinterpret_cast<void*>(&get_plugin_path)
	)->l_name;
}
static
string
get_plugin_dir()
{
	char *str = strdup(get_plugin_path().c_str());
	string s = dirname(str);
	free(str);
	return s;
}
/* Linker interfaces: direct interaction. */

/* The linker's interface for adding symbols from a claimed input file.  */
enum ld_plugin_status
(*add_symbols) (void *handle, int nsyms,
                          const struct ld_plugin_symbol *syms);

/* The linker's interface for getting the input file information with
   an open (possibly re-opened) file descriptor.  */
enum ld_plugin_status
(*get_input_file) (const void *handle,
                             struct ld_plugin_input_file *file);

enum ld_plugin_status
(*get_view) (const void *handle, const void **viewp);

/* The linker's interface for releasing the input file.  */
enum ld_plugin_status
(*release_input_file) (const void *handle);

/* The linker's interface for retrieving symbol resolution information.  */
enum ld_plugin_status
(*get_symbols) (const void *handle, int nsyms,
                          struct ld_plugin_symbol *syms);

/* The linker's interface for adding a compiled input file.  */
enum ld_plugin_status
(*add_input_file) (const char *pathname);

/* The linker's interface for adding a library that should be searched.  */
enum ld_plugin_status
(*add_input_library) (const char *libname);

/* The linker's interface for adding a library path that should be searched.  */
enum ld_plugin_status
(*set_extra_library_path) (const char *path);

/* The linker's interface for issuing a warning or error message.  */
enum ld_plugin_status
(*message) (int level, const char *format, ...);

/* The linker's interface for retrieving the number of sections in an object.
   The handle is obtained in the claim_file handler.  This interface should
   only be invoked in the claim_file handler.   This function sets *COUNT to
   the number of sections in the object.  */
enum ld_plugin_status
(*get_input_section_count) (const void* handle, unsigned int *count);

/* The linker's interface for retrieving the section type of a specific
   section in an object.  This interface should only be invoked in the
   claim_file handler.  This function sets *TYPE to an ELF SHT_xxx value.  */
enum ld_plugin_status
(*get_input_section_type) (const struct ld_plugin_section section,
                                     unsigned int *type);

/* The linker's interface for retrieving the name of a specific section in
   an object. This interface should only be invoked in the claim_file handler.
   This function sets *SECTION_NAME_PTR to a null-terminated buffer allocated
   by malloc.  The plugin must free *SECTION_NAME_PTR.  */
enum ld_plugin_status
(*get_input_section_name) (const struct ld_plugin_section section,
                                     char **section_name_ptr);

/* The linker's interface for retrieving the contents of a specific section
   in an object.  This interface should only be invoked in the claim_file
   handler.  This function sets *SECTION_CONTENTS to point to a buffer that is
   valid until clam_file handler returns.  It sets *LEN to the size of the
   buffer.  */
enum ld_plugin_status
(*get_input_section_contents) (const struct ld_plugin_section section,
                                         const unsigned char **section_contents,
                                         size_t* len);

/* The linker's interface for specifying the desired order of sections.
   The sections should be specifed using the array SECTION_LIST in the
   order in which they should appear in the final layout.  NUM_SECTIONS
   specifies the number of entries in each array.  This should be invoked
   in the all_symbols_read handler.  */
enum ld_plugin_status
(*update_section_order) (const struct ld_plugin_section *section_list,
				   unsigned int num_sections);

/* The linker's interface for specifying that reordering of sections is
   desired so that the linker can prepare for it.  This should be invoked
   before update_section_order, preferably in the claim_file handler.  */
enum ld_plugin_status
(*allow_section_ordering) (void);

/* The linker's interface for specifying that a subset of sections is
   to be mapped to a unique segment.  If the plugin wants to call
   unique_segment_for_sections, it must call this function from a
   claim_file_handler or when it is first loaded.  */
enum ld_plugin_status
(*allow_unique_segment_for_sections) (void);

/* The linker's interface for specifying that a specific set of sections
   must be mapped to a unique segment.  ELF segments do not have names
   and the NAME is used as the name of the newly created output section
   that is then placed in the unique PT_LOAD segment.  FLAGS is used to
   specify if any additional segment flags need to be set.  For instance,
   a specific segment flag can be set to identify this segment.  Unsetting
   segment flags that would be set by default is not possible.  The
   parameter SEGMENT_ALIGNMENT when non-zero will override the default.  */
enum ld_plugin_status
(*unique_segment_for_sections) (
    const char* segment_name,
    uint64_t segment_flags,
    uint64_t segment_alignment,
    const struct ld_plugin_section * section_list,
    unsigned int num_sections);

/* The linker's interface for retrieving the section alignment requirement
   of a specific section in an object.  This interface should only be invoked in the
   claim_file handler.  This function sets *ADDRALIGN to the ELF sh_addralign
   value of the input section.  */
enum ld_plugin_status
(*get_input_section_alignment) (const struct ld_plugin_section section,
                                          unsigned int *addralign);

/* The linker's interface for retrieving the section size of a specific section
   in an object.  This interface should only be invoked in the claim_file handler.
   This function sets *SECSIZE to the ELF sh_size
   value of the input section.  */
enum ld_plugin_status
(*get_input_section_size) (const struct ld_plugin_section section,
                                     uint64_t *secsize);

/* Handlers that the linker lets us register. */

/* The plugin library's "claim file" handler.  */
static
enum ld_plugin_status
claim_file_handler (
  const struct ld_plugin_input_file *file, int *claimed)
{
	debug_printf(1, "claim-file handler called (%s, currently %d)\n", file->name, *claimed);

	/* Don't claim any files if we're generating relocatable output.
	 * We only affect final links. */
	if (output_file_type == LDPO_REL) { *claimed = 0; return LDPS_OK; }
	
	/* If we "claim" a file, we are responsible for feeding its contents
	 * to the linker.
	 */
	bool should_claim = false;
	if (should_claim)
	{
		*claimed = 1;
		//claimed_files.push_back(make_pair(file, tmpname));
	}
	
	return LDPS_OK;
}

/* The plugin library's "all symbols read" handler.  */
static
enum ld_plugin_status
all_symbols_read_handler (void)
{
	debug_printf(1, "all-symbols-read handler called ()\n");
	/* How is this done in, say, the LLVM LTO plugin?
	 * In the claim-file hook, it just claims files and grabs input data.
	 * In the all-symbols-read hook, it creates lots of temporary files
	 *  and does codegen.
	 * How does it feed the generated code back to the linker?
	 * It generates temporary object files and uses add_input_file()
	 * to add them to the link.
	 */
	
	/* Things we can do in here:
	/* 
		(*add_symbols) (void *handle, int nsyms,
                        		  const struct ld_plugin_symbol *syms);

		(*get_input_file) (const void *handle,
                            		 struct ld_plugin_input_file *file);

		(*get_view) (const void *handle, const void **viewp);

		(*release_input_file) (const void *handle);

		(*get_symbols) (const void *handle, int nsyms,
                        		  struct ld_plugin_symbol *syms);

		(*add_input_file) (const char *pathname);

		(*add_input_library) (const char *libname);
	 */
	
	static string stubs = get_plugin_dir() + "/stubs.o";
	static string runtime = get_plugin_dir() + "/runtime.so";
	
	// for (auto p : claimed_files) add_input_file(p.name.c_str());
	
	/* Also add the extra input files. */
	add_input_file(stubs.c_str());
	add_input_file(runtime.c_str());
	
	return LDPS_OK;
}

/* The plugin library's cleanup handler.  */
static
enum ld_plugin_status
cleanup_handler (void)
{
	debug_printf(1, "cleanup handler called ()\n");
	
	// for (auto p : claimed_files) release_input_file(p.input_file);

	return LDPS_OK;
}

/* Linker interfaces: hook registration. */

/* The linker's interface for registering the "claim file" handler.  */
enum ld_plugin_status
(*register_claim_file) (ld_plugin_claim_file_handler handler);
/* The linker's interface for registering the "all symbols read" handler.  */
enum ld_plugin_status
(*register_all_symbols_read) (
  ld_plugin_all_symbols_read_handler handler);
/* The linker's interface for registering the cleanup handler.  */
enum ld_plugin_status
(*register_cleanup) (ld_plugin_cleanup_handler handler);

/* The plugin library's "onload" entry point.  */
extern "C" {
enum ld_plugin_status
onload(struct ld_plugin_tv *tv);
}
enum ld_plugin_status
onload(struct ld_plugin_tv *tv)
{
	if (getenv("DEBUG_CC"))
	{
		debug_level = 1;
	}
	// for debugging
	if (getenv("LD_DELAY_STARTUP"))
	{
		debug_printf(1, "Hello from linker plugin, in pid %d\n", getpid());
		sleep(12);
	}

#define CASE(x) \
	case LDPT_ ## x: debug_printf(1, "Transfer vector contained LDPT_" #x ", arg %p\n", i_tv->tv_u.tv_string); break;
#define CASE_INT(x, dest) \
	case LDPT_ ## x: debug_printf(1, "Transfer vector contained LDPT_" #x ", arg %d\n", i_tv->tv_u.tv_val); dest = i_tv->tv_u.tv_val; break;
#define CASE_STRING(x) \
	case LDPT_ ## x: debug_printf(1, "Transfer vector contained LDPT_" #x ", arg `%s'\n", i_tv->tv_u.tv_string); break;
#define CASE_FP(x, lc) \
	case LDPT_ ## x: debug_printf(1, "Transfer vector contained LDPT_" #x "; argument %p\n", \
	    i_tv->tv_u.tv_ ## lc); \
	lc = static_cast<__typeof(lc)>(i_tv->tv_u.tv_ ## lc); \
	break;
#define CASE_FP_REGISTER(x, lc) \
	case LDPT_REGISTER_ ## x: debug_printf(1, "Transfer vector contained LDPT_REGISTER_" #x "; argument %p\n", \
	    i_tv->tv_u.tv_register_ ## lc); \
	register_ ## lc = static_cast<__typeof(register_ ## lc)>(i_tv->tv_u.tv_register_ ## lc); \
	break;
	for (struct ld_plugin_tv *i_tv = tv; i_tv->tv_tag != LDPT_NULL; ++i_tv)
	{
		switch (i_tv->tv_tag)
		{
			CASE(NULL)
			CASE_INT(API_VERSION, api_version)
			CASE_INT(GOLD_VERSION, gold_version)
			CASE_INT(LINKER_OUTPUT, output_file_type)
			CASE_STRING(OPTION)
			CASE_FP_REGISTER(CLAIM_FILE_HOOK, claim_file)
			CASE_FP_REGISTER(ALL_SYMBOLS_READ_HOOK, all_symbols_read)
			CASE_FP_REGISTER(CLEANUP_HOOK, cleanup)
			CASE_FP(ADD_SYMBOLS, add_symbols)
			CASE_FP(GET_SYMBOLS, get_symbols)
			CASE_FP(ADD_INPUT_FILE, add_input_file)
			CASE_FP(MESSAGE, message)
			CASE_FP(GET_INPUT_FILE, get_input_file)
			CASE_FP(RELEASE_INPUT_FILE, release_input_file)
			CASE_FP(ADD_INPUT_LIBRARY, add_input_library)
			CASE_STRING(OUTPUT_NAME)
			CASE_FP(SET_EXTRA_LIBRARY_PATH, set_extra_library_path)
			CASE_INT(GNU_LD_VERSION, gnu_ld_version)
			CASE_FP(GET_VIEW, get_view)
			CASE_FP(GET_INPUT_SECTION_COUNT, get_input_section_count)
			CASE_FP(GET_INPUT_SECTION_TYPE, get_input_section_type)
			CASE_FP(GET_INPUT_SECTION_NAME, get_input_section_name)
			CASE_FP(GET_INPUT_SECTION_CONTENTS, get_input_section_contents)
			CASE_FP(UPDATE_SECTION_ORDER, update_section_order)
			CASE_FP(ALLOW_SECTION_ORDERING, allow_section_ordering)
			CASE_FP(GET_SYMBOLS_V2, get_symbols)
			CASE_FP(ALLOW_UNIQUE_SEGMENT_FOR_SECTIONS, allow_unique_segment_for_sections)
			CASE_FP(UNIQUE_SEGMENT_FOR_SECTIONS, unique_segment_for_sections)
			CASE(GET_SYMBOLS_V3)
			CASE_FP(GET_INPUT_SECTION_ALIGNMENT, get_input_section_alignment)
			CASE_FP(GET_INPUT_SECTION_SIZE, get_input_section_size)
			default:
				debug_printf(1, "Did not recognise transfer vector element %d\n", 
					(int) i_tv->tv_tag);
				break;
		}
	}
	
	if (register_claim_file) register_claim_file(claim_file_handler);
	if (register_all_symbols_read) register_all_symbols_read(all_symbols_read_handler);
	if (register_cleanup) register_cleanup(cleanup_handler);
	
	/* Do our local initialization. */
	
	return LDPS_OK;
}
