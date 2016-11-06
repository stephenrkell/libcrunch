#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

int main(void)
{
	char pad1[200];
	int is[] = { 0, 1, 2, 3, 4 };
	char pad2[200];

	int *oneprev = &is[-1];
	printf("Got %d\n", *oneprev);

	return 0;
}
