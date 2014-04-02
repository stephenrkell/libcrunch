#include <stdio.h>
#include <assert.h>

long f(long i, long *arg)
{
	return i + (unsigned long long) arg;
}

int main(void)
{
	void *fake = &f;
	
	/* Want a cast that will exercise check_args but nevertheless succeed. */
	int *(*fake_fun)(void *, void *) = (int *(*)(void*, void*)) fake;
	
	long l = 42;
	
	/* We're allowed to pass a pointer as the first arg; it's effectively 
	 * cast to a long, but without a syntactic cast -- just by calling a 
	 * type-mismatched function. However, there's no harm done, so no error.  */
	int *recovered = fake_fun(&fake, &l);

	printf("It says: %p\n", recovered);

	return 0;
}
