/* This is a simple dwarfpp program which generates a C file
 * recording data on a uniqued set of data types  allocated in a given executable.
 */
 
#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <string>
#include <cctype>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/graph_concepts.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/algorithm/string.hpp>
#include <srk31/algorithm.hpp>
#include <srk31/ordinal.hpp>
#include <dwarfpp/spec_adt.hpp>
#include <dwarfpp/adt.hpp>
#include <fileno.hpp>

#include "helpers.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::map;
using boost::make_shared;
using std::ios;
using std::ifstream;
using boost::dynamic_pointer_cast;
using boost::optional;
//using boost::shared_ptr;
using std::ostringstream;
using namespace dwarf;
using boost::filesystem::path;
using dwarf::spec::compile_unit_die;
using dwarf::spec::type_die;
using dwarf::spec::with_data_members_die;
using dwarf::spec::with_dynamic_location_die;
using dwarf::spec::type_chain_die;

// this encodes only the uniqued set of types, not the relations between them!
typedef std::map< uniqued_name, shared_ptr< spec::type_die > > master_relation_t;
typedef std::map< pair< string, unsigned long >, uniqued_name > allocsites_relation_t;
typedef std::set< shared_ptr< spec::subprogram_die > > subprograms_list_t;

master_relation_t::key_type
key_from_type(shared_ptr<type_die> t);

namespace boost 
{
	// specialise the boost graph_traits class for encap::dieset
	template <>
	struct graph_traits<master_relation_t> {
		//typedef master_relation_t::value_type vertex_descriptor;
		/* We need to be able to assign to vertex descriptors -- not because the
		 * algorithms will attempt to update entries in the underlying map, but because
		 * they will instantiate vertex descriptors in other structures, e.g. the
		 * topsorted container. */
		typedef pair< pair<string, string> , master_relation_t::mapped_type>
		    /*nonconst_*/vertex_descriptor;
		typedef vertex_descriptor nonconst_vertex_descriptor;

		typedef shared_ptr<dwarf::spec::member_die> edge_descriptor;
		
		/* To iterate through out-edges, we just do the usual member children
		 * iterator. */
		typedef dwarf::spec::with_data_members_die::member_iterator out_edge_iterator;

		/* To iterate through vertices, we just iterate through the map. */
		typedef master_relation_t::const_iterator vertex_iterator;
		
		typedef directed_tag directed_category;
		typedef allow_parallel_edge_tag edge_parallel_category;

		struct traversal_tag :
		  public virtual vertex_list_graph_tag,
		  public virtual incidence_graph_tag { };
		typedef traversal_tag traversal_category;
		
		typedef unsigned vertices_size_type;
		typedef unsigned edges_size_type;
		typedef unsigned degree_size_type;
	};

	graph_traits<master_relation_t>::vertex_descriptor
	source(
		graph_traits<master_relation_t>::edge_descriptor e,
		const master_relation_t& g
	)
	{
		// edge descriptor is a shared_ptr<member_die>
		// so we just look up the *containing* type's uniqued-name in the graph
		auto raw_parent = e->get_parent();
		assert(raw_parent);
		auto parent = dynamic_pointer_cast<with_data_members_die>(raw_parent);
		assert(parent);
		auto enclosing_cu = e->enclosing_compile_unit();
		assert(enclosing_cu);
		auto parent_type = dynamic_pointer_cast<type_die>(parent);
		assert(parent_type);
		auto uniqued_name = key_from_type(parent_type);
		
		auto found = g.find(uniqued_name);
		assert(found != g.end());
		return *found;
	}

	graph_traits<master_relation_t>::vertex_descriptor
	target(
		graph_traits<master_relation_t>::edge_descriptor e,
		const master_relation_t& g
	)
	{
		// edge descriptor is a shared_ptr<member_die>
		// so we just look up the *contained* type's uniqued-name in the graph
		auto t = e->get_type();
		assert(t);
		auto concrete_t = t->get_concrete_type();
		assert(concrete_t);
		auto enclosing_cu = concrete_t->enclosing_compile_unit();
		assert(enclosing_cu);
		auto uniqued_name = key_from_type(concrete_t);
		
		auto found = g.find(uniqued_name);
		assert(found != g.end());
		return *found;
	}
	
	inline std::pair<
		graph_traits<master_relation_t>::out_edge_iterator,
		graph_traits<master_relation_t>::out_edge_iterator >  
	out_edges(
		graph_traits<master_relation_t>::vertex_descriptor u, 
		const master_relation_t& g)
	{
		/* If we're a with_data_members die, out edges are the
		 * member children. Otherwise, our out edges are empty. */
		auto with_data_members = dynamic_pointer_cast<with_data_members_die>(u.second);
		if (with_data_members)
 		{
			return make_pair(
				with_data_members->member_children_begin(),
				with_data_members->member_children_end()
			);
		}
		else
		{
			auto member_children_end = with_data_members_die::member_iterator(
				with_data_members_die::member_transform_iterator(
					with_data_members_die::member_filter_iterator(
						u.second->children_end(), 
						u.second->children_end()
					), 
					boost::dynamic_pointer_cast<spec::member_die>
				)
			);

			return make_pair(
				member_children_end,
				member_children_end
			);
		}
	}
	
	inline graph_traits<master_relation_t>::degree_size_type
	out_degree(
		graph_traits<master_relation_t>::vertex_descriptor u,
		const master_relation_t& g)
	{
		/* If we're a with_data_members die, out edge count is the
		 * count of member children. Otherwise, it's zero. */
		auto with_data_members = dynamic_pointer_cast<with_data_members_die>(u.second);
		if (with_data_members)
		{
			auto seq = out_edges(u, g);
			return srk31::count(seq.first, seq.second);
		}
		else
		{
			return 0;
		}
	}

	inline std::pair<
		graph_traits<master_relation_t>::vertex_iterator,
		graph_traits<master_relation_t>::vertex_iterator >  
	vertices(const master_relation_t& g)
	{
		return make_pair(g.begin(), g.end());
	}	
	inline graph_traits<master_relation_t>::vertices_size_type 
	num_vertices(const master_relation_t& g)
	{
		return g.size();
	}
}

typedef std::vector<
	boost::graph_traits<master_relation_t>::nonconst_vertex_descriptor
> container;

void print_uniqtypes_output(const master_relation_t& g, const container& c);
void print_stacktypes_output(const subprograms_list_t& l);
void print_allocsites_output(const allocsites_relation_t& r);
#define BIGGEST_EQUIV 6
	typedef const char *equiv_class_t[BIGGEST_EQUIV];
	equiv_class_t equivs[] = {
		{ "signed char", "char", NULL},
		{ "signed int", "int", "signed", NULL },
		{ "unsigned int", "unsigned", NULL },
		{ "signed long int", "long signed int", "signed long", "long", "long int", NULL },
		{ "unsigned long int", "long unsigned int", "unsigned long", NULL },
		{ "signed long long int", "long long signed int", "signed long long", "long long int", "long long", NULL },
		{ "unsigned long long int", "long long unsigned int", "unsigned long long", NULL }
	};

master_relation_t::key_type
key_from_type(shared_ptr<type_die> t)
{
	uniqued_name n;
	t = t->get_concrete_type();
	if (t->get_tag() != DW_TAG_pointer_type)
	{
		auto cu = t->enclosing_compile_unit();

		string file_to_use = (t->get_decl_file() && *t->get_decl_file() != 0) 
		                                     ? cu->source_file_name(*t->get_decl_file()) : "";
		
		// for named base types, we use equivalence classes
		string name_to_use; 
		if (!t->get_name() || t->get_tag() != DW_TAG_base_type)
		{
			name_to_use = t->get_name() ? *t->get_name() : offset_to_string(t->get_offset());
		}
		else // t->get_name() && t->get_tag == DW_TAG_base_type
		{
			string name_to_search_for = *t->get_name();
			// search equiv classes for a type of this name
			for (equiv_class_t *i_equiv = &equivs[0]; i_equiv < &equivs[sizeof equivs / sizeof(equiv_class_t)]; ++i_equiv)
			{
				for (const char **i_el = i_equiv[0]; *i_el != NULL; ++i_el)
				{
					assert(i_el < i_equiv[BIGGEST_EQUIV]);
					if (name_to_search_for == string(*i_el))
					{
						name_to_use = (*i_equiv)[0];
						break;
					}
				}
				if (name_to_use != "") break; // we've got it
			}
			
			// if we've still not got it....
			if (name_to_use == "") name_to_use = name_to_search_for;
		}

		assert(name_to_use != "char"); // ... for example. It should be "signed char" of course!
		n = make_pair(file_to_use, name_to_use);
	}
	else // DW_TAG_pointer_type
	{
		shared_ptr<type_die> opt_target_type = dynamic_pointer_cast<spec::pointer_type_die>(t)->get_type();
		if (opt_target_type) opt_target_type = opt_target_type->get_concrete_type();
		string opt_target_type_name;
		if (!opt_target_type) opt_target_type_name = "void";
		else
		{
			opt_target_type_name = opt_target_type->get_name() ? 
				*opt_target_type->get_name() 
			: offset_to_string(opt_target_type->get_offset());
		}
		/* We roll with: no header file, and the name with __PTR_ for '^'/
		 * OR, HMM, the header file of the ultimate pointee? YES, it should be this.
		 * Let's do it. */
		int levels_of_indirection = 0;
		shared_ptr<type_die> ultimate_pointee_type = t->get_concrete_type();
		shared_ptr<type_chain_die> type_chain;
		do
		{
			type_chain = dynamic_pointer_cast<type_chain_die>(ultimate_pointee_type);
			if (type_chain) 
			{
				++levels_of_indirection;
				ultimate_pointee_type = type_chain->get_type();
				if (ultimate_pointee_type) ultimate_pointee_type = ultimate_pointee_type->get_concrete_type();
			}
		} while (type_chain);
		
		assert(levels_of_indirection >= 1);
		
		string defining_header;
		if (!ultimate_pointee_type)
		{
			// we have the "void" type, possibly indirected over multiple levels
			defining_header = "";
		}
		else 
		{
			defining_header = 
			(ultimate_pointee_type->get_decl_file() && *ultimate_pointee_type->get_decl_file() != 0) 
			   ? ultimate_pointee_type->enclosing_compile_unit()->source_file_name(
			      *ultimate_pointee_type->get_decl_file()) 
			   : "";
		}

		string target_typename_to_use = opt_target_type ? key_from_type(opt_target_type).second : "void";
		
		ostringstream os(std::ios::out | std::ios::binary);
		std::ostream_iterator<char, char> oi(os);
		
		// here we are translating a dumpallocs-style type descriptor name...
		// ... into a uniqtypes-style name. BUT WHY? 
		//boost::regex_replace(oi, s.begin(), s.end(),
		//	boost::regex("(\\^)"), "(__PTR_)", 
		//	boost::match_default | boost::format_all);
		//assert(os.str() != "char"); // ... for example
		
		for (int i = 0; i < levels_of_indirection; ++i)
		{
			os << "__PTR_";
		}
		os << target_typename_to_use;
		
		n = make_pair(defining_header, os.str());
	}
	
	return n;
}

shared_ptr<type_die>
find_type_in_cu(shared_ptr<compile_unit_die> p_cu, const string& name)
{
	/* For the most part, we just do named_child.
	 * BUT, for base types, we widen the search, using our equivalence classes. */
	for (equiv_class_t *i_equiv = &equivs[0]; i_equiv < &equivs[sizeof equivs / sizeof(equiv_class_t)]; ++i_equiv)
	{
		for (const char **i_el = i_equiv[0]; *i_el != NULL; ++i_el)
		{
			assert(i_el < i_equiv[BIGGEST_EQUIV]);
			if (name == string(*i_el))
			{
				/* We try every element in the class */
				for (const char **i_attempt = i_equiv[0]; *i_attempt != NULL; ++i_attempt)
				{
					auto found = p_cu->named_child(string(*i_attempt));
					auto found_type = dynamic_pointer_cast<type_die>(found);
					if (found_type) return found_type;
				}
			}
		}
	}
#undef BIGGEST_EQUIV
	// if we got here, just try named_child
	return dynamic_pointer_cast<type_die>(p_cu->named_child(name)); //shared_ptr<type_die>();
}

uniqued_name recursively_add_type(shared_ptr<spec::type_die> t, master_relation_t& r)
{
	if (!t) return make_pair("", "");
	t = t->get_concrete_type();
	
	/* If it's a base type, we might not have a decl_file, */
	if (!t->get_decl_file() || *t->get_decl_file() == 0)
	{
		if (t->get_tag() != DW_TAG_base_type
		 && t->get_tag() != DW_TAG_pointer_type
		 && t->get_tag() != DW_TAG_array_type)
		{
			cerr << "Warning: skipping non-base non-pointer non-array type described by " << *t //
			//if (t->get_name()) cerr << t->get_name();
			//else cerr << "(unknown, offset: " << std::hex << t->get_offset() << std::dec << ")";
			/*cerr */ << " because no file is recorded for its definition." << endl;
			return make_pair("", "");
		}
		// else it's a base type, so we go with the blank type
		// FIXME: should canonicalise base types here
		// (to the same as the ikind/fkinds come out from Cil.Pretty)
	}
	uniqued_name n = key_from_type(t);
	
	boost::smatch m;
	//uniqued_name n = key_from_type(t);
	//if (r.find(n) != r.end())
	//cerr << "adding type " << n.second << " defined (or declared? FIXME) in file " << n.first << endl;
	if (r.find(n) != r.end()
		&& t->get_tag() != DW_TAG_base_type
		&& !boost::regex_match(n.second, m, boost::regex(".*__PTR_.*")))
	{
		//cerr << "warning: non-base non-pointer type named " << n.second << " already exists!" << endl;
	}
	r[n] = t;
	
	/* Now recurse on members */
	auto has_data_members = dynamic_pointer_cast<with_data_members_die>(t);
	if (!has_data_members) return n;
	for (auto i_child = has_data_members->member_children_begin();
		i_child != has_data_members->member_children_end(); ++i_child)
	{
		recursively_add_type((*i_child)->get_type(), r);
	}
	
	return n;
}

int main(int argc, char **argv)
{
	/* We read from stdin lines such as those output by dumpallocs,
	 * prefixed by their filename. Actually they will have been 
	 * stored in .allocsites files. */ 
	
	map<string, shared_ptr<ifstream> > ifstreams;
	map<string, shared_ptr<lib::file> > files;
	map<string, shared_ptr<lib::dieset> > diesets;
	
	boost::shared_ptr<ifstream> p_in;
	if (argc > 1) 
	{
		p_in = boost::make_shared<ifstream>(argv[1]);
		if (!*p_in) 
		{
			cerr << "Could not open file " << argv[1] << endl;
			return 1;
		}
	}
	std::istream& in = p_in ? *p_in : cin;
	
	master_relation_t master_relation;
	allocsites_relation_t allocsites_relation;
	subprograms_list_t subprograms_list;
	
	char buf[4096];
	string objname;
	string symname;
	unsigned file_addr;
	string sourcefile; 
	unsigned line;
	unsigned end_line;
	string alloc_typename;
	while (in.getline(buf, sizeof buf - 1)
		&& 0 == read_allocs_line(string(buf), objname, symname, file_addr, sourcefile, line, end_line, alloc_typename))
	{
		/* Open the dieset */
		if (ifstreams.find(objname) == ifstreams.end())
		{
			ifstreams.insert(make_pair(objname, make_shared<ifstream>(objname)));
			if (!*ifstreams[objname]) return 1;
			try
			{
				files.insert(make_pair(objname, make_shared<lib::file>(fileno(*ifstreams[objname]))));
				assert(files[objname]);
				diesets.insert(make_pair(objname, make_shared<lib::dieset>(*files[objname])));
				assert(diesets[objname]);
			} 
			catch (dwarf::lib::Error e)
			{
				// libdwarf(pp)? problem
				cerr << "libdwarf(pp)? error: " << dwarf::lib::dwarf_errmsg(e.e) << endl;
				return 1;
			}
		}
		// now assert we have what we need
		assert(ifstreams.find(objname) != ifstreams.end());
		assert(files.find(objname) != files.end());
		assert(diesets.find(objname) != diesets.end());		
		
		/* alloc_typename is in C declarator form.
		   What to do about this?
		   HACK: for now, support only a limited set of cases:
		   IDENT
		   IDENT '*'+
		   
		   AND delete the tokens "const", "volatile", "struct" and "union" first!
		   HACK: we are not respecting the C struct/union namespacing here. OH well.
		 */
		
		string nonconst_typename = alloc_typename;
		const char *to_delete[] = { "const", "volatile", "struct", "union" };
		for (int i = 0; i < srk31::array_len(to_delete); ++i)
		{
			size_t pos = 0;
			size_t foundpos;
			while ((foundpos = nonconst_typename.find(to_delete[i], pos)) != string::npos) 
			{
				/* Is this a well-bounded match, i.e. not part of a token? 
				 * - start must be beginning-of-string or following a non-a-zA-Z0-9_ char 
				 * - end must be end-of-string or followed by a non-a-zA-Z0-9_ char */
				size_t endpos = foundpos + string(to_delete[i]).length();
				if (
					(foundpos == 0 || (!isalnum(nonconst_typename[foundpos - 1]) 
					               &&  '_' != nonconst_typename[foundpos - 1] ))
				  && 
					(endpos == nonconst_typename.length()
					|| (!isalnum(nonconst_typename[endpos] || '_' != nonconst_typename[endpos])))
					)
				{
					/* it's a proper match -- delete that string and then start in the same place */
					nonconst_typename.replace(foundpos, endpos - foundpos, "");
					pos = foundpos;
				}
				else
				{
					/* It's not a proper match -- advance past this match. */
					pos = foundpos + 1;
				}
			}
		}
		//cerr << "After nonconsting, typename " << alloc_typename << " is " << nonconst_typename << endl;
		string clean_typename;
		
		boost::smatch match;
		// HACK: we allow embedded spaces to allow "unsigned int" et al
		const boost::regex ident("[[:blank:]]*([a-zA-Z_][a-zA-Z0-9_ ]*)[[:blank:]]*");
		//const boost::regex ident_ptr("[[:blank:]]*([a-zA-Z_][a-zA-Z0-9]*)(([[:blank:]]*\\*)*)[[:blank:]]*");
		if (boost::regex_match(nonconst_typename, match, ident))
		{
			clean_typename = match[0];
		}
		//else if (boost::regex_match(nonconst_typename, match, ident_ptr))
		else if (boost::regex_match(nonconst_typename, match, boost::regex("^(\\^+)(.*)")))
		{
			// this is a pointer. we need to fix on a single typename for these guys,
			// then (below) look in the dieset for an instance,
			// and assuming it's found (FIXME: it might not be present, theoretically)
			// generate it a unique name
			
			// with the new caret-based name, it's already clean! 
			clean_typename = nonconst_typename;
			
// 			clean_typename = match[0];
// 			unsigned stars_count = 0; 
// 			size_t pos = 0; 
// 			size_t foundpos;
// 			string matched_string = match[1];
// 			while ((foundpos = matched_string.find_first_of("*", pos)) != string::npos)
// 			{
// 				++stars_count;
// 				++foundpos;
// 			}
// 			for (int i = 0; i < stars_count; ++i) clean_typename += '*';
		}
		else if (boost::regex_match(nonconst_typename, match, boost::regex("\\$FAILED\\$")))
		{
			cerr << "skipping unidentified type at allocsite " 
			     << objname << "<" << symname << ">" 
				 << "@ 0x" << std::hex << file_addr << std::dec << endl;
			continue;
		}
		else
		{
			cerr << "warning: bad typename " << nonconst_typename 
				<< " from " << sourcefile << ":" << line << "-" << end_line << endl;
			continue;
		}
		boost::trim(clean_typename);
		
		/* Build the containment structure and topsort it. 
		 * It only needs to reflect the allocated types. So,
		 * traverse the type depthfirst. */
		
		shared_ptr<compile_unit_die> found_cu;
		optional<path> found_sourcefile_path;
		shared_ptr<type_die> found_type;
		/* Find a CU such that 
		 - one of its source files is named sourcefile, taken relative to comp_dir if necessary;
		 - that file defines a type of the name we want
		 */ 
		
		std::vector<shared_ptr<compile_unit_die> > embodying_cus;
		
		for (auto i_cu = diesets[objname]->toplevel()->compile_unit_children_begin();
			 i_cu != diesets[objname]->toplevel()->compile_unit_children_end();
			 ++i_cu)
		{
// 			/* Add this CU's subprograms to the subprograms list */
// 			for (auto i_subp = (*i_cu)->subprogram_children_begin();
// 			          i_subp != (*i_cu)->subprogram_children_end();
// 			        ++i_subp)
// 			{
// 				// FIXME: what if subprograms are not immediate children of their CU?
// 				
// 				// only add real, defined subprograms to the list
// 				if ( 
// 						( !(*i_subp)->get_declaration() || !*(*i_subp)->get_declaration() )
// 				   )
// 				{
// 					subprograms_list.insert(*i_subp);
// 				}
// 			}
		
			if ((*i_cu)->get_name() && (*i_cu)->get_comp_dir())
			{
				auto cu_die_name = *(*i_cu)->get_name();
				auto cu_comp_dir = *(*i_cu)->get_comp_dir();
				
				for (unsigned i_srcfile = 1; i_srcfile <= (*i_cu)->source_file_count(); i_srcfile++)
				{
					/* Does this source file have a matching name? */
					path current_sourcepath;
					string cu_srcfile_mayberelative = (*i_cu)->source_file_name(i_srcfile);
					//cerr << "CU " << *(*i_cu)->get_name() << " sourcefile " << i_srcfile << " is " <<
					//	cu_srcfile_mayberelative << endl;
					if (!path(cu_srcfile_mayberelative).has_root_directory())
					{ 
						current_sourcepath = path(cu_comp_dir) / path(cu_srcfile_mayberelative);
					}
					else current_sourcepath = path(cu_srcfile_mayberelative);
					
					// FIXME: smarter search
					// FIXME: look around a bit, since sizeof isn't enough to keep DIE in the object file
					if (current_sourcepath == path(sourcefile))
					{ 
						// YES this CU embodies the source file, so we can search for the type
						embodying_cus.push_back(*i_cu);
						
						// handle pointers here
						// HACK: find *any* pointer type,
						// and use that, with empty sourcefile.
						if (clean_typename.size() > 0 && *clean_typename.begin() == '^')
						{
							if ((*i_cu)->pointer_type_children_begin()
								== (*i_cu)->pointer_type_children_end())
							{
								cerr << "Warning: no pointer type children in CU! Trying the next one." << endl;
								continue;
							}
							else
							{
								found_cu = *i_cu;
								found_type = *(*i_cu)->pointer_type_children_begin();
								found_sourcefile_path = current_sourcepath;
								goto cu_loop_exit;
							}
						}
						else
						{
							found_type = find_type_in_cu(*i_cu, clean_typename);
							if (found_type/* && (
										found_type->get_tag() == DW_TAG_base_type ||
										(found_type->get_decl_file()
											&& *found_type->get_decl_file() == i_srcfile))*/)
							{
								found_cu = *i_cu;
								found_sourcefile_path = current_sourcepath;
								goto cu_loop_exit;
							}
							else found_type = shared_ptr<type_die>();
						}
					}
				}
			}
		}
		
	cu_loop_exit:
		if (!found_type)
		{
			cerr << "Warning: no type named " << clean_typename 
				<< " in CUs (found " << embodying_cus.size() << ":";
				for (auto i_cu = embodying_cus.begin(); i_cu != embodying_cus.end(); ++i_cu)
				{
					if (i_cu != embodying_cus.begin()) cerr << ", ";
					cerr << *(*i_cu)->get_name();
				}
				cerr << ") embodying " 
				<< sourcefile << ":" << line << "-" << end_line
				<< " (allocsite: " << objname 
				<< "<" << symname << "> @" << std::hex << file_addr << std::dec << ">)" << endl;
			continue;
		}
		// now we found the type
		//cerr << "SUCCESS: found type: " << *found_type << endl;
		
		uniqued_name name_used = recursively_add_type(found_type, master_relation);
		
		// add to the allocsites table too
		// recall: this is the mapping from allocsites to uniqtype addrs
		// the uniqtype addrs are given as idents, so we just have to use the same name
		allocsites_relation.insert(
			make_pair(
				make_pair(objname, file_addr),
				name_used
			)
		);
		
	} // end while
	
	// concept checks for graph
	boost::function_requires< 
		boost::InputIterator<
			boost::graph_traits<master_relation_t>::out_edge_iterator
		>
	> ();
	boost::function_requires< boost::IncidenceGraphConcept<master_relation_t> >();
	
	// now topsort
	std::map<
		boost::graph_traits<master_relation_t>::nonconst_vertex_descriptor, 
		boost::default_color_type
	> underlying_topsort_node_color_map;
	auto topsort_color_map = boost::make_assoc_property_map( // ColorMap provides a mutable "Color" property per node
		underlying_topsort_node_color_map
	);
	auto named_params = boost::color_map(topsort_color_map);

	container topsorted_container;
	boost::topological_sort(
		master_relation, 
		std::back_inserter(topsorted_container), 
		named_params
	);
	
	// now print .c file in topsorted order
	print_uniqtypes_output(master_relation, topsorted_container);
	// print_stacktypes_output(subprograms_list);
	print_allocsites_output(allocsites_relation);
	
	// success! 
	return 0;
}

void print_uniqtypes_output(const master_relation_t& g, const container& c)
{
	/* For each type we output a record:
	 * - a pointer to its name;
	 * - a length prefix;
	 * - a list of <offset, included-type-record ptr> pairs.
	 */

	cout << "struct rec \n\
{ \n\
	const char *name; \n\
	unsigned sz; \n\
	unsigned len; \n\
	struct { \n\
		unsigned offset; \n\
		struct rec *ptr; \n\
	} contained[]; \n\
};\n";
	/* DWARF doesn't reify void, but we do. So output a rec for void first of all. */
	cout << "\n/* uniqtype for void */\n";
	cout << "struct rec " << mangle_typename(make_pair(string(""), string("void")))
		<< " = {\n\t\"" << "void" << "\",\n\t"
		<< "0" << " /* sz " << "(void) */,\n\t"
		<< "0" << " /* len */,\n\t"
		<< "/* contained */ { }\n};";

	for (auto i_vert = c.begin(); i_vert != c.end(); ++i_vert)
	{
		auto opt_sz = i_vert->second->calculate_byte_size();
		if (!opt_sz)
		{
			// we have an incomplete type
			cerr << "Warning: type " 
				<< i_vert->first.second
				<< " is incomplete, treated as zero-size." << endl;
		}
		if (i_vert->first.second == string("void"))
		{
			cerr << "Warning: skipping explicitly declared void type from CU "
				<< *i_vert->second->enclosing_compile_unit()->get_name()
				<< endl;
			continue;
		}
		
		cout << "\n/* uniqtype for " << i_vert->first.second 
			<< " defined in " << i_vert->first.first << " */\n";
		cout << "struct rec " << mangle_typename(i_vert->first)
			<< " = {\n\t\"" << i_vert->first.second << "\",\n\t"
			<< (opt_sz ? *opt_sz : 0) << " /* sz " << (opt_sz ? "" : "(incomplete) ") << "*/,\n\t"
			<< boost::out_degree(*i_vert, g) << " /* len */,\n\t"
			<< /* contained[0] */ "/* contained */ {\n\t\t";
		auto out_edges = boost::out_edges(*i_vert, g);
		unsigned i_member = 0;
		std::set<lib::Dwarf_Unsigned> used_offsets;
		for (auto i_edge = out_edges.first; i_edge != out_edges.second; ++i_edge)
		{
			++i_member;
		
			/* if we're not the first, write a comma */
			if (i_edge != out_edges.first) cout << ",\n\t\t";
			
			/* begin the struct */
			cout << "{ ";
			
			// compute offset
			lib::Dwarf_Unsigned offset;
			if ((*i_edge)->get_data_member_location() 
				&& (*i_edge)->get_data_member_location()->size() > 0)
			{
				
				if ((*i_edge)->get_data_member_location()->size() > 1)
				{
					cerr << "Warning: ignoring all but first location expression for "
						<< i_member << srk31::ordinal_suffix(i_member) << " data member of " 
						<< i_vert->first.second << endl;
				}
				offset = lib::evaluator(
					(*i_edge)->get_data_member_location()->at(0), (*i_edge)->get_ds().get_spec(),
					// push zero as the initial stack value
					std::stack<lib::Dwarf_Unsigned>(std::deque<lib::Dwarf_Unsigned>(1, 0UL))).tos();
			}
			else 
			{
				cerr << "Warning: "
					<< i_member << srk31::ordinal_suffix(i_member) << " data member of " 
					<< i_vert->first.second 
					<< " has no location description; assuming zero-offset." 
					<< endl;
				offset = 0UL;
			}
			// check whether this subobject overlaps another
			if (used_offsets.find(offset) != used_offsets.end())
			{
				// FIXME: do overlapment check
				cerr << "Warning: "
					<< i_member << srk31::ordinal_suffix(i_member) << " data member of " 
					<< i_vert->first.second 
					<< " shares offset with a previous member." 
					<< endl;
			}
			
			cout << offset << ", ";
			
			// compute and print destination name
			cout << "&" << mangle_typename(boost::target(*i_edge, g).first);
			
			// end the struct
			cout << " }";
		}
		cout << "\n\t}"; /* end contained */
		cout << "\n};\n"; /* end struct rec */
	}
}

void print_stacktypes_output(const subprograms_list_t& l)
{
	/* For each subprogram, for each vaddr range for which its
	 * stack frame is laid out differently, output a uniqtype record.
	 * We do this by
	 * - collecting all local variables and formal parameters on a depthfirst walk;
	 * - collecting their vaddr ranges into a partition, splitting any overlapping ranges
	     and building a mapping from each range to the variables/parameters valid in it;
	 * - when we're finished, outputting a distinct uniqtype for each range;
	 * - also, output a table of IPs-to-uniqtypes.  */
	for (auto i_subp = l.begin(); i_subp != l.end(); ++i_subp)
	{
		typedef pair<lib::Dwarf_Unsigned, lib::Dwarf_Unsigned> interval_t;
		std::set< interval_t > intervals; // CU- or file-relative?
		// what is the complete vaddr range of this subprogram?
		interval_t subp_interval;
		// if we have a high PC and a low PC, use those
		if ((*i_subp)->get_high_pc() && (*i_subp)->get_low_pc())
		{
			subp_interval = make_pair((lib::Dwarf_Unsigned) (*i_subp)->get_low_pc()->addr,
			                          (lib::Dwarf_Unsigned) (*i_subp)->get_high_pc()->addr);
		}
		else
		{
			assert(false);
		}
		
// HACK while our iterator interfaces don't directly provide a depth method
#define GET_DEPTH(i)  ((i).base().path_from_root.size())
		auto start_dfs = (*i_subp)->iterator_here(); // gets a *dfs* iterator
		unsigned start_depth = GET_DEPTH(start_dfs);
		for (auto i_dfs = start_dfs; 
		    i_dfs == start_dfs || GET_DEPTH(i_dfs) > start_depth;
		    ++i_dfs)
		{
			shared_ptr<with_dynamic_location_die> wdl
			 = dynamic_pointer_cast<with_dynamic_location_die>(*i_dfs);
			if (wdl)
			{
				// HACK: further splitting shouldn't be necessary, but is for now.
				// we actually really need it to be either a variable or a fp
				if (wdl->get_tag() == DW_TAG_formal_parameter)
				{
					
				}
				else if (wdl->get_tag() == DW_TAG_variable)
				{
					
				}
			}
		}
#undef GET_DEPTH
	}
}

void print_allocsites_output(const allocsites_relation_t& r)
{
	cout << "struct allocsite_entry\n\
{ \n\
	void *next; \n\
	void *prev; \n\
	void *allocsite; \n\
	struct rec *uniqtype; \n\
};\n";	

	cout << "struct allocsite_entry allocsites[] = {" << endl;
	for (auto i_site = r.begin(); i_site != r.end(); ++i_site)
	{
		if (i_site != r.begin()) cout << ",";
		
		cout << "\n\t/* allocsite info for " << i_site->first.first << "+"
			<< std::hex << "0x" << i_site->first.second << std::dec << " */";
		cout << "\n\t{ (void*)0, (void*)0, "
			<< "(char*) " << "__LOAD_ADDR_" 
			<< boost::to_upper_copy(mangle_objname(i_site->first.first))
			<< " + " << i_site->first.second << "UL, " 
			<< "&" << mangle_typename(i_site->second)
			<< " }";
	}
	// output a null terminator entry
	if (r.size() > 0) cout << ",";
	cout << "\n\t{ (void*)0, (void*)0, (void*)0, (struct rec *)0 }";
	
	// close the list
	cout << "\n};\n";
}
