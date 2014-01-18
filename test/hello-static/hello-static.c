#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

static int blah = 42;

int main(void)
{
	void *fake = &blah;

	int *recovered = (int *) fake;

	printf("It says: %d\n", *recovered);

	return 0;
}
