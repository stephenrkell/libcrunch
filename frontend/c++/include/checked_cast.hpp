#include <utility>

// the class of uniqtypes
template <typename T>
struct uniqtype
{
	static struct {} obj;
};

extern "C"
{
	extern int __is_a_internal(const void *obj, const void *uniqtype);
}

/*

If we want checked_static_cast to be a drop-in replacement for static_cast,
we're going to need some trickery, because static_cast's "template arguments"
only include the return type. How about

checked_cast< (omitted) >::static_cast<To, From> 

where From is defaulted to (omitted)? 

NO, because class template arguments are never automatically deduced.

What about

- a template constructor in the checked_cast<> class...

- ... which implicitly decays to the required type? 

YES, this seems to work (notes below).

*/

template <typename To>
struct checked_static_cast_t
{
	To temp; 
	operator To() const { return std::move(temp); }
	
	// from a temporary
	template <class From>
	checked_static_cast_t(const From& arg) : temp(static_cast<To>(arg)) {}
	// from is usually via a temporary...
	template <class From>
	static To from(const From& f) { return checked_static_cast_t(from).operator To(); }
	
	// from a temporary -- rvalue ref case
	template <class From>
	checked_static_cast_t(From&& arg) : temp(static_cast<To>(std::move(arg))) {}
	
	// from is usually via a temporary... -- rvalue ref case
	template <class From>
	static To&& from(From&& f) { return checked_static_cast_t(std::move(from)).operator To(); }
	
	// but from a built-in type is not
	static constexpr To from(int f) { return static_cast<To>(f); }
	static constexpr To from(unsigned int f) { return static_cast<To>(f); }
	static constexpr To from(char f) { return static_cast<To>(f); }
	static constexpr To from(unsigned char f) { return static_cast<To>(f); }
};

// the pointer case
template <typename ToPointerTarget>
struct checked_static_cast_t<ToPointerTarget*>
{
	ToPointerTarget *temp; 
	operator ToPointerTarget*() const 
	{ 
		// FIXME: assert
		__is_a_internal(temp, &uniqtype<ToPointerTarget>::obj);
		return std::move(temp); 
	}
	
	// from a temporary
	template <class From>
	checked_static_cast_t(const From& arg) : temp(static_cast<ToPointerTarget*>(arg)) {}
	
	// from is usually via a temporary...
	template <class From>
	static ToPointerTarget* from(const From& f) { return checked_static_cast_t(from).operator ToPointerTarget*(); }
	
	// from a temporary -- rvalue ref case
	template <class From>
	checked_static_cast_t(From&& arg) : temp(static_cast<ToPointerTarget*>(std::move(arg))) {}
	
	// from is usually via a temporary... -- rvalue ref case
	template <class From>
	static ToPointerTarget*&& from(From&& f) { return checked_static_cast_t(std::move(from)).operator ToPointerTarget*(); }
	
	// we CANNOT static cast to a pointer from a non-pointer
	//static constexpr To from(int f) { return static_cast<To>(f); }
	//static constexpr To from(unsigned int f) { return static_cast<To>(f); }
	//static constexpr To from(char f) { return static_cast<To>(f); }
	//static constexpr To from(unsigned char f) { return static_cast<To>(f); }
};

// static casts *to* an int are always constexpr and unchecked
template <>
struct checked_static_cast_t<int>
{
	int temp; 
	constexpr operator int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<int>(arg)) {}

	template <class From>
	static constexpr int from(const From& f) { return static_cast<int>(f); }
};
template <>
struct checked_static_cast_t<unsigned int>
{
	unsigned int temp; 
	constexpr operator unsigned int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<unsigned int>(arg)) {}

	template <class From>
	static constexpr unsigned int from(const From& f) { return static_cast<unsigned int>(f); }
};
template <>
struct checked_static_cast_t<short int>
{
	short int temp; 
	constexpr operator short int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<short int>(arg)) {}

	template <class From>
	static constexpr short int from(const From& f) { return static_cast<short int>(f); }
};
template <>
struct checked_static_cast_t<unsigned short int>
{
	unsigned short int temp; 
	constexpr operator unsigned short int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<unsigned short int>(arg)) {}

	template <class From>
	static constexpr unsigned short int from(const From& f) { return static_cast<unsigned short int>(f); }
};
template <>
struct checked_static_cast_t<long long int>
{
	long long temp; 
	constexpr operator long long int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<long long int>(arg)) {}

	template <class From>
	static constexpr long long int from(const From& f) { return static_cast<long long int>(f); }
};
template <>
struct checked_static_cast_t<unsigned long long int>
{
	unsigned long long int temp; 
	constexpr operator unsigned long long int() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<unsigned long long int>(arg)) {}

	template <class From>
	static constexpr unsigned long long int from(const From& f) { return static_cast<unsigned long long int>(f); }
};
template <>
struct checked_static_cast_t<char>
{
	char temp; 
	constexpr operator char() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<char>(arg)) {}
	
	template <class From>
	static constexpr char from(const From& f) { return static_cast<char>(f); }
};
template <>
struct checked_static_cast_t<unsigned char>
{
	unsigned char temp; 
	constexpr operator unsigned char() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<unsigned char>(arg)) {}

	template <class From>
	static constexpr unsigned char from(const From& f) { return static_cast<unsigned char>(f); }
};
template <>
struct checked_static_cast_t<wchar_t>
{
	wchar_t temp; 
	constexpr operator wchar_t() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<wchar_t>(arg)) {}

	template <class From>
	static constexpr wchar_t from(const From& f) { return static_cast<wchar_t>(f); }
};
template <>
struct checked_static_cast_t<bool>
{
	bool temp; 
	constexpr operator bool() const { return temp; }
	
	// from a temporary
	template <class From>
	constexpr checked_static_cast_t(const From& arg) : temp(static_cast<bool>(arg)) {}

	template <class From>
	static constexpr bool from(const From& f) { return static_cast<bool>(f); }
};

// 
// template <typename To>
// struct checked_reinterpret_cast_t
// {
// 	To temp; 
// 	template <class From>
// 	checked_reinterpret_cast_t(const From& arg) : temp(reinterpret_cast<To>(arg)) {}
// 	
// 	operator To() const { return temp; }
// };

/* Problem with the above: static_cast results need to be constexpr, at least in 
 * some cases.
 * 
 * Workaround: use an overload that is constexpr for every builtin type.
 * 
 * Problem: it also affects constexpr functions in the standard library,
 * e.g. std::_Ios_Fmtflags.
 * 
 * Workaround: use an explicit specialisation.
 * 
 * Problem: that explicit specialisation has to be a partial one.
 * 
 * Workaround: move the load to the class template.
 * 
 * Problem: function template still needs to be constexpr! 
 * 
 * Workaround: constexpr overloads as before
 *
 * Problem: doesn't solve static_cast<int>(__b) for user-defined type __b
 * 
 * Workaround: back to the temporary approach? Can we have constexpr constructors? YES.
 */

#define static_cast checked_static_cast_t
