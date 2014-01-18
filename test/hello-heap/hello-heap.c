#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

int main()
{
	int *blah = (int *) malloc(200 * sizeof (int));
	for (int i = 0; i < 200; ++i) blah[i] = 42;
	
	void *fake = blah;

	int *recovered = (int *) fake;

	printf("It says: %d\n", recovered[0]);

	free(blah);
	
	return 0;
}
