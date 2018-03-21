#include <stdio.h>

const char *f(void)
{
	static const char *s = "Blah!";
	
	return s;
}

const char *temp;

int main(void)
{
	temp = f();
	printf("Now I say: %s\n", temp + 2);
	return 0;
}
