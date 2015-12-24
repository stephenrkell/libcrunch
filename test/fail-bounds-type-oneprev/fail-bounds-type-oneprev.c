#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

/* PROBLEMS exposed by this test case and its variants:
 * 
 * "bad cast before adjust"
       for (float *pf = ((float*) i) - 1; pf >= (float*) &is[0]; --pf)
	   -- see fail-bounds-type-backin

 * "bad cast after adjust"
	   for (float *pf = (float*) (i - sizeof (float)); pf > (float*) &is[0]; --pf)
	   -- see bounds-type-simple

 * "compare one-prev with bad-cast"
	    for (float *pf = (float*) (i - sizeof (float)); pf >= (float*) &is[0]; --pf)
	    - i.e. the same as above but with ">=" and not ">" ^-^ here
	
	    - what happens: we trap the pointer from the last "--pf", because it's one-prev.
	
	       + OTHER small problems:
	            1. "max_bounds" (in libcrunch.c) might not be sensible
	            2. stack frame types are unions, i.e. have repeated offsets -- is this correct?

	    - what should happen: sloppily accommodate the one-prev, like with bounds-type-simple,
	          iff the users asks for it

 * "cast trapped pointer":
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
	for (float *pf = (float*) (i - sizeof (float)); pf >= (float*) &is[0]; --pf) // "adjust before [good] cast"
	// for (float *pf = (float*) (i - 5 * sizeof (float)); pf < (float*) &is[5]; ++pf) // "adjust before [good] cast"
	// for (float *pf = (float*) (i - 5 * sizeof (float)); pf < (float*) &is[5]; ++pf) // "adjust before [good] cast"
	{
		printf("About to deref: %p\n", pf);
		printf("Saw a float: %f\n", *pf);
	}
	
	/* HMM. What should happen here?
	 * We convert the one-past pointer to an integer.
	 * Then we convert it back to a float*.
	 * It isn't pointing at a float, so the cast fails.
	 * The code is arguably correct, though, because we're not going to use it
	 * until we've decremented it.
	 * Decrementing it currently doesn't work, because 
	 * doing arithmetic on the untrapped pointer confuses __fetch_bounds,
	 * -- it's not pointing at a float-typed area, and it's not a one-past trapped case.
	 * 
	 * We need to trap the bad pointer *at the site of the cast*. 
	 * Probably we should use a new kind of trapped pointer (LIBCRUNCH_TRAP_WRONG_TYPE).
	 * 
	 * Then we ideally want __fetch_bounds to handle the case where
	 * arithmetic is used to bring a bad-type pointer back in bounds.
	 * This is *sloppy* and should only be enabled on request.
	 * This is "[bad] cast,then adjust" instead of "adjust, then [good] cast".
	 * If we'd written
	 *                   float *pf = (float*) (i - sizeof (float))
	 * we'd be okay.
	 * 
	 * Bad-type trapping would reduce our ability to continue running code that 
	 * does bad casts, UNLESS we also catch SEGV and emulate those accesses. 
	 * That is probably the right thing to do.
	 * For the moment, we don't have many false positives, so we should I think
	 * suck it up and maybe even not bother with the SEGV handler.
	 * 
	 * Casting a pointer should de-trap it first -- even (especially for)
	 * casts whose target type means they need no check?
	 * HMM. "Same type" means leave trappedness.
	 *      "New type, checked" means clear trappedness.
	 *      "New type, void* or integer" means clear trappedness
	 *      "New type, char*" means... what? Okay to leave, I think (we ignore
	 *          trappedness when comparing pointers for <= < == != >= >).
	 *      "New type, GPP" means... what? again, we clear its trappedness.
	 * 
	 * Should we emulate all such uses of trapped pointers? YES, probably. 
	 * For now, doing arithmetic on a bad-cast pointer aborts the program. */
	
	return 0;
}
