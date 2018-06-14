#include <stdio.h>
#include <stdarg.h>

void doit(int n, ...)
{
	va_list ap;
	va_start(ap, n);
	vprintf("%d: ", ap);
	va_end(ap);
}

int main(void)
{
	printf("It says :");
	doit(42, 42);
	return 0;
}
