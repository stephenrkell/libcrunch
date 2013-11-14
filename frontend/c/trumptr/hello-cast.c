#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

static const char blah[] = "Hello, programmer.";

int main()
{
	void *fake = &blah;

	const char *recovered = (const char *) fake;

	printf("%s\n", recovered);

	return 0;
}
