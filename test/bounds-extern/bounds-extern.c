#include <stdio.h>

extern int blah[];

int indir_func(int *p)
{
	return p[41];
}

int *indir_store;

int main(void)
{
	indir_store = blah;
	printf("It is: %d\n", indir_func(blah));
	printf("It is: %d\n", indir_store[41]);
	printf("It is: %d\n", blah[41]);
	return 0;
}
