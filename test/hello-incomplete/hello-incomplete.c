#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libcrunch.h>

/* Our incomplete handling should handle correctly the case 
 * where a library, allocating a non-incomplete type, passes
 * it to a client where said type is incomplete, and the client
 * does checks on it. */

struct Foo; // defined in library
void *create_foo(void);

int main(void)
{
	__libcrunch_check_init();
	
	struct Foo *f = create_foo();
	
	return 0;
}
