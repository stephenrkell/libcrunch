#include <stdio.h>

int main(void)
{
	int is[42][23] = { [0] = { [0] = 0 } };
	
	/* Annoyingly, CIL expands the definition above
	 * into 
	 * - 42 pointers to arrays of 23 elements
	 * - a 23-iteration while loop, each deriving a pointer to a 42-integer array
	 * - a 42-iteration while loop, each iteration doing the following:
	 *     - zeroing out the first element of is[i]
	 *     - re-derive the pointer to is[i], and check that derivation
	 *     - for k in 1..22 (unrolled), check-derive *(&is[i]) + k
	 *           and write 0 into that pointer.
	 * 
	 * We want this all to collapse away to nothingness. How?
	 * 
	 * - each __check_derive call must pass bounds.
	 * - those bounds come from the size of the array
	 * - we can do this, with some fiddling.
	 * 
	 * We also currently fail the computation of is[i] for i == 1
	 * 
	 
	 bounds-multidim: code at 0x407cf5 generated an out-of-bounds pointer 0x7fffffffbe58 
	 (from 0x7fffffffbda0; difference 184; lb 0x7fffffffbda0; ub 0x7fffffffbe48)
	 
	(gdb) print &is
	$4 = (int (*)[42][23]) 0x7fffffffbda0
	(gdb) print &is[0][0]
	$5 = (int *) 0x7fffffffbda0
	(gdb) print &is[1][0]
	$6 = (int *) 0x7fffffffbdfc
	(gdb) print &is[2][0]
	$6 = (int *) 0x7fffffffbe58

	 * i.e. the first one is okay because it's one-past;
	 * the second one is not okay because it's far out.
	 */
	
	
	int *p = &is[0][0];
	
	p += 43;
	
	// this one is okay to dereference
	fprintf(stderr, "Got address %p, value %d\n", p, *p);
	
	return 0;
}
