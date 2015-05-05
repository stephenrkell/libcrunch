#include <stdio.h>
#include <stddef.h>
#include <assert.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following. */
	int is[] = { 5, 4, 3, 2 };
	
	int *a = &is[4];
	int *b = &is[0];
	
	ptrdiff_t d = (a - b);
	assert(d == 4);
	
	printf("Difference is %ld\n", (long) d);

	return 0;
}
