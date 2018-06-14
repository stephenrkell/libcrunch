#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

char *f(void *arg)
{
	char *pc = (char*) arg + 1;
	*pc = 42;
	return pc;
}

int main(void)
{
	char *ret = f(malloc(42));
	return ((intptr_t) ret) % *ret;
}
