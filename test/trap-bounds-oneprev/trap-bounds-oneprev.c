#include <stdio.h>
#include <stdint.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following. */
	
	int is[2] = { 42, 43 };
	
	uintptr_t sneaky = (uintptr_t) &is[0];
	sneaky -= sizeof (int);
	int *i_sneaky = (int *) sneaky;
	int *i_post = &is[2];
	int *i_pre = &is[-1];

	return 0;
}

/* 
struct {
	char *p;
} local_struct;

local_struct.p = &as[-1]
		
FIXME: local arrays of pointers
 */
