#include <stdio.h>
#include <assert.h>
#include "libcrunch.h"

static char blah[] = "blah!";

int main(void)
{
	void *fake = &blah;

	const char *recovered = (const char *) fake;

	printf("It says: %s\n", recovered);

	/* The point of this test used to be that libcrunch should
	 * NOT be inited. Now that __libcrunch_global_init is a
	 * constructor, we need some other way. */
	assert(__libcrunch_begun == 0);

	return 0;
}
