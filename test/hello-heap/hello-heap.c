#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

int main()
{
	const char *blah = (const char *) malloc(200 * sizeof (char));
	strncpy(blah, "Hello, programmer.", 200 * sizeof (char));
	
	void *fake = blah;

	const char *recovered = (const char *) fake;

	printf("%s\n", recovered);

	free(blah);
	
	return 0;
}
