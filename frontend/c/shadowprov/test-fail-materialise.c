#include <stdio.h>
int main(void)
{
	char *safe_byte = (char*) main;
	printf("Got as far as reading one byte: %d\n", *safe_byte);
	fflush(stdout);
	char *a_byte = (char*) 0x410000;
	return *a_byte;
}
