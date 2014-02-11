#include <stdio.h>
#include <assert.h>
#include "libcrunch.h"

int main(void)
{
	/* NOTE: the point of this test case is that libcrunch SHOULD 
	 * be inited, because the strengthening of void is checked. */
	struct foo {
		int bar;
	} *blah = malloc(sizeof (struct foo));
	
	assert(__libcrunch_is_initialized);

	return 0;
}
