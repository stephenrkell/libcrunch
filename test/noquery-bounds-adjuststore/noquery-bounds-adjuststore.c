#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* Check we can adjust a pointer and store it somewhere
 * without the __store_pointer_nonlocal causing a call to __fetch_bounds_ool. 
 * This is about picking up the locally cached bounds for the 
 * __cil_adjexpr_NN temporary. */

extern unsigned long __libcrunch_fetch_bounds_called;

int main(void)
{
	int x[20] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
	              10, 11, 12, 13, 14, 15, 16, 17, 18, 19 };
	int *px[20];
	int **p_xs = px;

	p_xs[0] = &x[0];
	for (int i = 1; i < 20; ++i)
	{
		p_xs[i] = p_xs[i-1] + 1;
	}

	printf("The 20th integer is: %d\n", *p_xs[19]);
	assert(__libcrunch_fetch_bounds_called == 0);
	return 0;
}
