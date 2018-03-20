#include <assert.h>

extern _Bool __libcrunch_is_initialized;

int main(int argc, char **argv)
{
	/* We should not instrument derefs, including those signified
	 * by indexing at zero. How can we tell whether a check was
	 * instrumented? */
	printf("argv[0] is %p\n", argv[0]);
	assert(!__libcrunch_is_initialized);

	/* The following is testing indexing of local arrays. It is
	 * based on a case in SPEC gcc. Previously crunchbound complained
	 * because the index expr in "info[n]" is not statically zero, yet we
	 * have inferred, from the use of sizeof, that we need only a singleton
	 * local bounds var for the array. This is because the sizing expression
	 * is in the compile-time-evaluable subset of C, but the indexing
	 * expression is not. But we already check for in-boundsness of local
	 * accesses, so any access to "info" will be bounds-checked, after
	 * which we know that accesses to its localbounds will be valid. */
	static const int nums[] = { 1 };
	#define N (sizeof nums / sizeof (int))
	struct { int *p; } *info[N];
	int n = 0;

	for (int n = 0; n < N; ++n)
	{
		info[n] = calloc(4, sizeof *info[0]);
	}
	/* return something that is always zero,
	 * but not statically knowably so. */
	return malloc_usable_size(info[0]) / 200000;
}

