#include <stdio.h>
#include <stdlib.h>

int vs[] = { 1, 2, 3 };
extern int other_vs[]; /* defined as an alias of vs, in mk.inc */

/* See noquery-bounds-static-init-ptr. 
 * The difference here is that some bounds initializers *will* require a query.
 */

int main(void)
{
	static int *arr[] = { &vs[0], &vs[1], &vs[2] };
	static struct { int *blah; } other_container = { &other_vs[0] };

	int n = rand();
	int sz = (sizeof arr / sizeof arr[0]);
	printf("Randomly read: %d\n", *(other_container.blah + rand() % sz));
	return 0;
}
