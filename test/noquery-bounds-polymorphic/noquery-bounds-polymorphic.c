/* We want to be able to treat pointers "polymorphically", i.e. 
 * via a void* lens,
 * without breaking the shadow space in the common case. */

#include <stdio.h>

void report_void(void *p)
{
	printf("Saw a void*: %p\n", p);
}

void frob_voids(void **pptr1, void **pptr2)
{
	if ((unsigned long) *pptr2 < (unsigned long) *pptr1)
	{
		printf("Frob means swap\n");
		/* swap them */
		void *tmp = *pptr2;
		*pptr2 = *pptr1;
		*pptr1 = tmp;
	}
}


int main(void)
{
	report_void(main);
	int xs[] = { 42, 69105 };
	int *ptr[] = { &xs[1], &xs[0] };
	frob_voids(&ptr[0], &ptr[1]);
	/* Now adjust one of them -- we should still get its bounds as if it's int*. */
	printf("After frobbing, *(ptr[0] + 1) is: %d\n", *(ptr[0] + 1));
	return 0;
}
