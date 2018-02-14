#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

/* "bad cast after adjust"
	   for (float *pf = (float*) (i - sizeof (float)); pf > (float*) &is[0]; --pf)
	   - what happens: we do arithmetic on a bad-type pointer,
	       but since the size matches (float vs int),
	       we get away with it
 */

int main(void)
{
	char pad1[200];
	int is[] = { 0, 1, 2, 3, 4 };
	char pad2[200];
	/* If we convert a trapped pointer to int, we should get a sensible value. */
	int *i1 = &is[5];
	unsigned long i = (unsigned long) i1;
	printf("The int we got is 0x%lx\n", i);
	assert(i - (unsigned long) &is[4] == sizeof (int));
	/* If we convert it back to a float* when *out of bounds*, it should not be usable,
	 * but __fetch_bounds might (ideally) let us sloppily accommodate it anyway.
	 * If we convert it back to an in-bounds float*, it should do whatever casting
	 * &is[4] to float* would normally do. This is to continue after printing a warning.
	 * Here we assume float and int have the same size. */
	for (float *pf = (float*) (i - sizeof (float)); pf > (float*) &is[0]; --pf) // "adjust before [good] cast"
	{
		printf("About to deref: %p\n", pf);
		printf("Saw a float: %f\n", *pf);
	}
	return 0;
}
