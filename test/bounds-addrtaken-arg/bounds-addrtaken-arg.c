#include <stdio.h>

static int is[] = { 1, 4, 9, 16 };
int f(int *p)
{
	printf("Somewhere in the array %p is %p\n", &p, p + 3);
	/* [The compiler thinks] printf might have modified p! */
	return p[3];
}

int main(void)
{
	int *local = is;
	printf("It says: %d\n", f(local));
	return 0;
}
