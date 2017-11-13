#include <stdio.h>
#include <stdarg.h>

void my_interp(const char *str, ...)
{
	va_list va;
	va_start(va, str);
	char c;
	int i;
	double d;
	while ('\0' != (c = *str++))
	{
		switch (c)
		{
			case 'i':
				i = va_arg(va, int);
				fprintf(stdout, "Seen in interp program: %d\n", i);
				break;
			case 'f':
				d = va_arg(va, double);
				fprintf(stdout, "Seen in interp program: %f\n", d);
				break;
			default:
				i = va_arg(va, int);
				fprintf(stdout, "Unrecognised char in interp program: '%c'\n", c);
				break;
		}
	}
	va_end(va);
}

int main(void)
{
	int i = 42;
	int j = 69105;
	double e = 2.71828;
	my_interp("ifi", i, e, j);
	return 0;
}
