#include <stdio.h>

int *outside;
int *another;
int **indirect = &another;

int main(void)
{
#ifndef LIBCRUNCH_NO_SHADOW_SPACE
	/* Test that we can store a pointer, load it again, and do some arithmetic
	 * on it, without hitting the slow path. */
	
	int is[100] = {
		[0] = 100,
		[99] = 1
	};
	int *pi = &is[0];
	
	outside = pi;
	printf("Having an effect for the sake of it...\n");
	int *loaded = outside;
	printf("Found: %d at loaded[99]\n", loaded[99]);
	
	*indirect = pi + 42;
	printf("Having an effect for the sake of it...\n");
	loaded = another;
	printf("Found: %d at another[-42]\n", another[-42]);
	
#endif
	return 0;
}
