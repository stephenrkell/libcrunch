#include <stdio.h>
#include <stdarg.h>

void my_interp(const char *str, ...)
{
	va_list va;
	va_start(va, str);
	char c;
	void *p;
	int *pi;
	float *pf;
	while ('\0' != (c = *str++))
	{
		switch (c)
		{
			case 'i':
				pi = va_arg(va, int*);
				fprintf(stdout, "Seen in interp program: %d\n", *pi);
				break;
			case 'f':
				pf = va_arg(va, float*);
				fprintf(stdout, "Seen in interp program: %f\n", *pf);
				break;
			default:
				p = va_arg(va, void*);
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
	float e = 2.71828;
	
	my_interp("fif", &i, &e, &j);
	
	return 0;
}
