#include <stdio.h>
#include <string.h>

int *g(void *p)
{
	return (int*)p;
}

int main(void)
{
	int x[2][2][47] = { { { 1, 2 }, { 3, 4 } },
	                   { { 5, 6 }, { 7, 8 } } };

	int dest[sizeof x / sizeof x[0]];

	/* Here we cast x to void*, spilling an int*-typed alias to the cache.
	 * It's essential that we collapse the type int[2][2][2] to int[8]
	 * at this point, because we don't generate uniqtypes of the
	 * form __uniqtype____ARRn___ARRm___x. */
	printf("Got back: %d\n", g(x)[1]);

	memcpy(dest, x, sizeof x);
	printf("First element in dest: %d\n", dest[0]);
	
	return 0;
}
