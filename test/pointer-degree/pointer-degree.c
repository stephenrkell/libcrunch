#include "libcrunch.h"

int main(void)
{
	void *blah1;
	void **blah2;
	void ***blah3;
	void ****blah4;
	int i;

	assert(!__is_a_pointer_of_degree_internal(&i, 0));
	assert(!__is_a_pointer_of_degree_internal(&i, 1));
	assert(!__is_a_pointer_of_degree_internal(&i, 2));

	assert(__is_a_pointer_of_degree_internal(&blah1, 1));

	assert(__is_a_pointer_of_degree_internal(&blah2, 1));
	assert(__is_a_pointer_of_degree_internal(&blah2, 2));

	assert(__is_a_pointer_of_degree_internal(&blah3, 1));
	assert(__is_a_pointer_of_degree_internal(&blah3, 2));
	assert(__is_a_pointer_of_degree_internal(&blah3, 3));

	assert(__is_a_pointer_of_degree_internal(&blah4, 1));
	assert(__is_a_pointer_of_degree_internal(&blah4, 2));
	assert(__is_a_pointer_of_degree_internal(&blah4, 3));
	assert(__is_a_pointer_of_degree_internal(&blah4, 4));

	assert(!__is_a_pointer_of_degree_internal(&blah1, 2));

	assert(!__is_a_pointer_of_degree_internal(&blah2, 3));

	assert(!__is_a_pointer_of_degree_internal(&blah3, 4));

	assert(!__is_a_pointer_of_degree_internal(&blah4, 5));

	return 0;

}
