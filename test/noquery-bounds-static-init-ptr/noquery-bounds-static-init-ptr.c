#include <stdio.h>
#include <stdlib.h>

int vs[] = { 1, 2, 3 };

/* Here we are testing the path in libcrunch.c which initializes the shadow space 
 * with bounds for all statically initialized pointers. If it succeeds, it will
 * be possible to do adjustments on such a pointer without resorting to a liballocs
 * query. */

int main(void)
{
	static int *arr[] = { &vs[0], &vs[1], &vs[2] };
	static struct { int *blah; } container = { &vs[0] };

	int n = rand();
	int sz = (sizeof arr / sizeof arr[0]);
	int idx = n % sz;
	printf("Randomly read: %d\n", *(arr[idx] + rand() % (sz - idx)));
	printf("Randomly read: %d\n", *(container.blah + rand() % sz));
	return 0;
}
