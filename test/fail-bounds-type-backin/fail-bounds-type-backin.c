#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

/* "bad cast before adjust" (1)
       for (float *pf = ((float*) i) - 1; pf >= (float*) &is[0]; --pf)
	   
	   - what happens: we try to do arithmetic on a bad pointer, trying to
	     bring it back into bounds...
	   - what should happen: 
	        pointer stays trapped and 1-bounded, 
	            generating one error report every time it does arithmetic on such a pointer,
	            using fault handler to allow it to continue?
	        pointer gets "rectified" when it is brought back into bounds,
	            i.e. we fetch new bounds after the bad arithmetic and see if they are good?
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
	for (float *pf = ((float*) i) - 1; pf > (float*) &is[0]; --pf) // "[bad] cast before adjust"
	{
		printf("About to deref: %p\n", pf);
		printf("Saw a float: %f\n", *pf);
	}
	
	return 0;
}
