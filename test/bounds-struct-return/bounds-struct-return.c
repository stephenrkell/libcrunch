#include <stdio.h>

struct contains_ptr
{
	int *do_not_care;
	long not_a_ptr;
	int *p;
};

struct contains_ptr callee(int *x)
{
	static int is[] = { 69105, 4432 };
	return (struct contains_ptr) { .do_not_care = x, .not_a_ptr = 42l, .p = is };
}

struct contains_ptr l;
int main(int argc, char **argv)
{
	l = callee(&argc);
	printf("It says: %d\n", *(l.p + 1));
	return 0;
}
