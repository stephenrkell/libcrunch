#include <stdio.h>

static int is[] = { 1, 4, 9, 16 };
int f(int **p)
{
	return (*p)[3];
}

int main(void)
{
	int *local = is;
	printf("It says: %d\n", f(&local));
	return 0;
}
