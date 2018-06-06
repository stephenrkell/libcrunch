#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void *f(void *arg)
{
	return (char*) arg + 1;
}

int main(void)
{
	void *block = f(malloc(42));
	return ((intptr_t) block) % 256;
}
