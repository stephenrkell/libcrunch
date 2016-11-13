#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following. */
	char *buf = calloc(1, 42);
	buf[41] = '\0';

	*(int*)&buf[20] = 42;

	return 0;
}
