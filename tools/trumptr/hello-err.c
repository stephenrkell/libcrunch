#include <stdio.h>
#include <errno.h>

int main(void)
{
	printf("Hello, world!\n");
	errno = 0;

	return errno;
}
