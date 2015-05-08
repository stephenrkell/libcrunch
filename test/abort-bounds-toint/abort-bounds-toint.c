#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

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
		
	/* If we convert it back to a float*, it should not be usable.
	 * Assume float is at least as big as int. */
	for (float *pf = (float*) i - 1; pf >= (float*) &is[0]; --pf)
	{
		printf("Saw a float: %f\n", *pf);
	}
	
	/* HMM. What should happen here? We get a cast failing, but we continue;
	 * Then doing arithmetic on the wrong-type pointer confuses __fetch_bounds. 
	 * Perhaps we should issue a trapped pointer (LIBCRUNCH_TRAP_WRONG_TYPE) 
	 * on a bad cast? That would reduce our ability to continue running code that 
	 * does bad casts, UNLESS we also catch SEGV and emulate those accesses. 
	 * That is probably the right thing to do. 
	 * Should we emulate all such uses of trapped pointers? YES, probably. 
	 * For now, doing arithmetic on a bad-cast pointer aborts the program. */
	
	return 0;
}
