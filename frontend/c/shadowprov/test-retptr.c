#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main(void)
{
	void *block = malloc(42);
	return ((intptr_t) block) % 256;
}
