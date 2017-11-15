#include <stdio.h>
#include <stdlib.h>

static int is[] = { 1, 4, 9, 16 };
int f(int *dummy, int *p)
{
	printf("Somewhere in the array %p (dummy is %p) is %p\n", &p, dummy, p + 3);
	/* [The compiler thinks] printf might have modified p! */
	return p[3];
}

int main(void)
{
	int *local = is;
	int *dummy = calloc(1, sizeof (int));
	printf("It says: %d\n", f(dummy, local));
	free(dummy);
	return 0;
}
