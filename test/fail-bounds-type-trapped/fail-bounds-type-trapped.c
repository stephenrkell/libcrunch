#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

/* "cast trapped pointer":
	   for (float *pf = (float*) (i - 5 * sizeof (float)); pf < (float*) &is[5]; ++pf) 
	   -- see fail-bounds-type-trapped
	   
	   - what happens: we call __is_aU on a trapped ptr, confusing things
	       ... giving storage and/or invalid-stackframe liballocs errors
	       but (consequently) no libcrunch errors
	   - what should happen: always de-trap calls to __is_aU?
	       flag up the liballocs errors more explicitly?
	       make crunchbound rewrite all pointer arithmetic to be on char* ?
	            -- would this help? if we did (char*)(float*)oob, it still does the cast.
	       just give up?
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
	// for (float *pf = ((float*) i) - 1; pf >= (float*) &is[0]; --pf) // "[bad] cast before adjust"
	for (float *pf = (float*) (i - 5 * sizeof (float)); pf < (float*) &is[5]; ++pf) // "adjust before [good] cast"
	{
		printf("About to deref: %p\n", pf);
		printf("Saw a float: %f\n", *pf);
	}
	
	return 0;
}
