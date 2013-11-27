#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

int main()
{
	const char blah[] = "Hello, programmer.";
	
	void *fake = &blah;

	const char *recovered = (const char *) fake;

	printf("%s\n", recovered);

	return 0;
}
