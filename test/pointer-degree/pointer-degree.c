#include "libcrunch.h"

extern unsigned long __libcrunch_failed;

int main(void)
{
	void *blah1;
	void **blah2;
	void ***blah3;
	void ****blah4;
	int i;

	/* HACK: because our assertions always evaluate true
     * at the moment, we can't write them as we'd like to.
	 * Instead, assert the failure count. */

	assert((/*!*/__is_a_pointer_of_degree_internal(&i, 0), __libcrunch_failed == 1));
	assert((/*!*/__is_a_pointer_of_degree_internal(&i, 1), __libcrunch_failed == 2));
	assert((/*!*/__is_a_pointer_of_degree_internal(&i, 2), __libcrunch_failed == 3));

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

	assert((/*!*/__is_a_pointer_of_degree_internal(&blah1, 2), __libcrunch_failed == 4));

	assert((/*!*/__is_a_pointer_of_degree_internal(&blah2, 3), __libcrunch_failed == 5));

	assert((/*!*/__is_a_pointer_of_degree_internal(&blah3, 4), __libcrunch_failed == 6));

	assert((/*!*/__is_a_pointer_of_degree_internal(&blah4, 5), __libcrunch_failed == 7));

	return 0;

}
