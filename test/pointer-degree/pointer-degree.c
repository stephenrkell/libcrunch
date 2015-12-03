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
	_Bool b0 = __is_a_pointer_of_degree_internal(&i, 0);
	assert(__libcrunch_failed == 1);
	_Bool b1 = __is_a_pointer_of_degree_internal(&i, 1);
	assert(__libcrunch_failed == 2);
	_Bool b2 = __is_a_pointer_of_degree_internal(&i, 2);
	assert(__libcrunch_failed == 3);

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

	_Bool p2 = __is_a_pointer_of_degree_internal(&blah1, 2);
	assert(__libcrunch_failed == 4);
	_Bool p3 = __is_a_pointer_of_degree_internal(&blah2, 3);
	assert(__libcrunch_failed == 5);
	_Bool p4 = __is_a_pointer_of_degree_internal(&blah3, 4);
	assert(__libcrunch_failed == 6);
	_Bool p5 = __is_a_pointer_of_degree_internal(&blah4, 5);
	assert(__libcrunch_failed == 7);

	return 0;

}
