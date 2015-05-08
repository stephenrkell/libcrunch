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
	
	/* If we convert it back to a char*, it should be usable. 
	 * ACTUALLY it's not obvious that this is true. Our cast
	 * logic needs to detect when we're casting to the "edge"
	 * of something, if there's no abutting object. */
	for (char *pc = (char*) i - sizeof (int); pc >= (char*) &is[0]; --pc)
	{
		printf("Saw a byte: %x\n", *pc);
	}
	
	return 0;
}

/* 
struct {
	char *p;
} local_struct;

local_struct.p = &as[-1]
		
FIXME: local arrays of pointers
 */
