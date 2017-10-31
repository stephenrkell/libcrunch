#include <assert.h>

extern _Bool __libcrunch_is_initialized;

int main(int argc, char **argv)
{
	/* We should not instrument derefs, including those signified
	 * by indexing at zero. How can we tell whether a check was
	 * instrumented? */
	printf("argv[0] is %p\n", argv[0]);
	assert(!__libcrunch_is_initialized);
	return 0;
}
