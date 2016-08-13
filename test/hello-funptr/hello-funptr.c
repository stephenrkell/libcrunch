#include <stdio.h>
#include <assert.h>

long f(long i, long *arg)
{
	return i + (unsigned long long) arg;
}

int main(void)
{
	void *fake = &f;
	
	/* Want a cast that will exercise check_args but nevertheless succeed
	 * *iff* we're using sloppy function pointers. */
	long (*fake_fun)(void *, void *) = (long(*)(void*, void*)) fake;
	
	long l = 42;
	
	/* We're allowed to pass a pointer as the first arg; it's effectively 
	 * cast to a long, but without a syntactic cast -- just by calling a 
	 * type-mismatched function. However, there's no harm done, so no error.  */
	long recovered = fake_fun(&fake, &l);

	printf("It says: %lx\n", recovered);

	return 0;
}
