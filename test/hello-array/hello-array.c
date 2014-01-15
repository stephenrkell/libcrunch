#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

struct foo
{
	int a;
	double b;
} blah[42];

int main(void)
{
	void *fake1 = &blah[41];

	struct foo *recovered1 = (struct foo *) fake1;
	
	void *fake2 = &blah[41].b;
	
	double *recovered2 = (double *) fake2;

	printf("Recovered %d\n", *recovered2);

	return 0;
}
