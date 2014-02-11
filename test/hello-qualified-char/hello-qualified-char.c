#include <stdio.h>
#include <assert.h>
#include "libcrunch.h"

static char blah[] = "blah!";

int main(void)
{
	void *fake = &blah;

	/* NOTE: the point of this test case is that libcrunch should 
	 * NOT be inited. */
	const char *recovered = (const char *) fake;

	printf("It says: %s\n", recovered);
	
	assert(!__libcrunch_is_initialized);

	return 0;
}
