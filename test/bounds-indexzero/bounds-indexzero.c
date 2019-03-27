#include <assert.h>
#include <stdlib.h>

extern unsigned long __libcrunch_fetch_bounds_called;

int xs[42] = { 0 };
extern int ys[] __attribute__((alias("xs")));

int main(int argc, char **argv)
{
	/* We should not instrument derefs, including those signified
	 * by indexing at zero. How can we tell whether we just did a
	 * check? I think the way to assert that we didn't do a check is
	 * (1) invent a check that will always do a query
	 * (2) check that no query happened
	 * (3) ... relying on the compiler to optimise away the unused query
	 * But I can't think of a check that will always do a query.
	 * Or can I? This is easier than I think. If we're in crunchb mode,
	 * it means we don't check derefs or statically-known-zero indexing.
	 * So we won't do any fetches that would be needed only for such checks.
	 */
	printf("argv[0] is %p\n", argv[0]);
	putenv("BLAHUNUSED=xyz");
	char *v = getenv("BLAHUNUSED");
	// if we could, we'd assert that we don't have bounds
	printf("ourvar[0] is %p\n", v[0]);
	//assert(!__libcrunch_is_initialized);
	printf("ys[0] is %d\n", ys[0]);
	assert(__libcrunch_fetch_bounds_called == 0);

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

