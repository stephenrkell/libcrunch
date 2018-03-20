#include <stdio.h>

struct contains_ptr
{
	int *p;
};

int callee(struct contains_ptr *p)
{
	printf("It says: %d\n", *(p->p + 1));
	return *(p->p + 1);
}

int main(int argc, char **argv)
{
	int is[] = { 69105, 4432 };
	struct contains_ptr l;
	l.p = is;
	return callee(&l) - 4432;
}
