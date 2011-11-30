/* This is a simple dwarfpp program which generates a C file
 * recording data on a uniqued set of data types  allocated in a given executable.
 */
 
#include <fstream>
#include <sstream>
#include <map>
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
using namespace dwarf;
using boost::filesystem::path;
using dwarf::spec::compile_unit_die;
using dwarf::spec::type_die;
using dwarf::spec::with_data_members_die;

// this encodes only the uniqued set of types, not the relations between them!
typedef std::map< uniqued_name, shared_ptr< spec::type_die > > master_relation_t;
typedef std::map< pair< string, unsigned long >, uniqued_name > allocsites_relation_t;

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
void print_allocsites_output(const allocsites_relation_t& r);

master_relation_t::key_type
key_from_type(shared_ptr<type_die> t)
{
	auto cu = t->enclosing_compile_unit();
	
	string file_to_use = (t->get_decl_file() && *t->get_decl_file() != 0) 
	                                      ? cu->source_file_name(*t->get_decl_file()) : "";
	string name_to_use = t->get_name() ? *t->get_name() : offset_to_string(t->get_offset());
	
	uniqued_name n = make_pair(file_to_use, name_to_use);
	
	return n;
}

void recursively_add_type(shared_ptr<spec::type_die> t, master_relation_t& r)
{
	if (!t) return;
	
	/* If it's a base type, we might not have a decl_file, */
	if (!t->get_decl_file() || *t->get_decl_file() == 0)
	{
		if (t->get_tag() != DW_TAG_base_type)
		{
			cerr << "Warning: skipping non-base type named ";
			if (t->get_name()) cerr << t->get_name();
			else cerr << "(unknown, offset: " << std::hex << t->get_offset() << std::dec << ")";
			cerr << " because no file is recorded for its declaration." << endl;
		}
	}
	
	uniqued_name n = key_from_type(t);
	if (r.find(n) != r.end())
	cerr << "adding type " << n.second << " defined (or declared? FIXME) in file " << n.first << endl;
	if (r.find(n) != r.end())
	cerr << "warning: type named " << n.second << " already exists!" << endl;
	r[n] = t;
	
	/* Now recurse on members */
	auto has_data_members = dynamic_pointer_cast<with_data_members_die>(t);
	if (!has_data_members) return;
	for (auto i_child = has_data_members->member_children_begin();
		i_child != has_data_members->member_children_end(); ++i_child)
	{
		recursively_add_type((*i_child)->get_type(), r);
	}
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
	
	char buf[4096];
	string objname;
	string symname;
	unsigned offset;
	string cuname; 
	unsigned line;
	string alloc_typename;
	while (in.getline(buf, sizeof buf - 1)
		&& 0 == read_allocs_line(string(buf), objname, symname, offset, cuname, line, alloc_typename))
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
		
		/* Build the containment structure and topsort it. 
		 * It only needs to reflect the allocated types. So,
		 * traverse the type depthfirst. */
		
		shared_ptr<compile_unit_die> found_cu;
		/* Find a CU such that its comp_dir + name == cuname. */ 
		for (auto i_cu = diesets[objname]->toplevel()->compile_unit_children_begin();
			 i_cu != diesets[objname]->toplevel()->compile_unit_children_end();
			 ++i_cu)
		{
			if ((*i_cu)->get_name() && (*i_cu)->get_comp_dir())
			{
				auto cu_die_name = *(*i_cu)->get_name();
				auto cu_comp_dir = *(*i_cu)->get_comp_dir();
				auto fullpath = path(cu_comp_dir) / path(cu_die_name);
				if (path(cuname) == fullpath)
				{
					// matched
					found_cu = *i_cu;
				}
			}
		}
		if (!found_cu)
		{
			cerr << "source file " << cuname << " had no match in " << objname << endl;
			continue;
		}
		
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
		cerr << "After nonconsting, typename " << alloc_typename << " is " << nonconst_typename << endl;
		string clean_typename;
		
		boost::smatch match;
		const boost::regex ident("[[:blank:]]*([a-zA-Z_][a-zA-Z0-9]*)[[:blank:]]*");
		const boost::regex ident_ptr("[[:blank:]]*([a-zA-Z_][a-zA-Z0-9]*)(([[:blank:]]*\\*)*)[[:blank:]]*");
		if (boost::regex_match(nonconst_typename, match, ident))
		{
			clean_typename = match[0];
		}
		else if (boost::regex_match(nonconst_typename, match, ident_ptr))
		{
			clean_typename = match[0];
			unsigned stars_count = 0; 
			size_t pos = 0; 
			size_t foundpos;
			string matched_string = match[1];
			while ((foundpos = matched_string.find_first_of("*", pos)) != string::npos)
			{
				++stars_count;
				++foundpos;
			}
			for (int i = 0; i < stars_count; ++i) clean_typename += '*';
		}
		else if (boost::regex_match(nonconst_typename, match, boost::regex("\\$FAILED\\$")))
		{
			cerr << "skipping unidentified type at allocsite " 
			     << objname << "<" << symname << ">" 
				 << "+0x" << std::hex << offset << std::dec << endl;
			continue;
		}
		else
		{
			cerr << "warning: didn't understand typename " << nonconst_typename << endl;
			continue;
		}
		
		// FIXME: smarter search
		// FIXME: look around a bit, since sizeof isn't enough to keep DIE in the object file
		auto found_type = found_cu->named_child(clean_typename);
		if (!found_type)
		{
			cerr << "CU " << *found_cu->get_name() << " did not define a type named " << clean_typename << endl;
			continue;
		}
		auto is_type = dynamic_pointer_cast<spec::type_die>(found_type);
		if (!is_type)
		{
			cerr << "CU " << *found_cu->get_name() << " element " << *found_type << " is not a type." << endl;
			continue;
		}
		// now we found the type
		cerr << "SUCCESS: found type: " << *is_type << endl;
		recursively_add_type(is_type, master_relation);
		
		// add to the allocsites table too
		allocsites_relation.insert(
			make_pair(
				make_pair(objname, offset),
				key_from_type(is_type)
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
		
		cout << "struct rec " << mangle_typename(i_vert->first)
			<< " = {\n\t\"" << i_vert->first.second << "\",\n\t"
			<< (opt_sz ? *opt_sz : 0) << " /* sz " << (opt_sz ? "" : "(incomplete) ") << "*/,\n\t"
			<< boost::out_degree(*i_vert, g) << " /* len */,\n\t"
			<< /* contained[0] */ "/* contained */ {\n\t\t";
		auto out_edges = boost::out_edges(*i_vert, g);
		for (auto i_edge = out_edges.first; i_edge != out_edges.second; ++i_edge)
		{
			/* if we're not the first, write a comma */
			if (i_edge != out_edges.first) cout << ",\n\t\t";
			/* begin the struct */
			cout << "{ ";
			
			// compute offset
			lib::Dwarf_Unsigned offset = lib::evaluator(
				(*i_edge)->get_data_member_location()->at(0), (*i_edge)->get_ds().get_spec(),
				// push zero as the initial stack value
				std::stack<lib::Dwarf_Unsigned>(std::deque<lib::Dwarf_Unsigned>(1, 0UL))).tos();
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

void print_allocsites_output(const allocsites_relation_t& r)
{
	cout << "typedef void *entry[2];" << endl;
	cout << "entry allocsites[] = {" << endl;
	for (auto i_site = r.begin(); i_site != r.end(); ++i_site)
	{
		if (i_site != r.begin()) cout << ",";
		
		cout << "\n\t{ "
			<< "(char*) " << "LOAD_ADDR_" 
			<< boost::to_upper_copy(mangle_objname(i_site->first.first))
			<< " + " << i_site->first.second << "UL, " 
			<< "&" << mangle_typename(i_site->second)
			<< "}";
	}
	cout << "\n};\n";
}
