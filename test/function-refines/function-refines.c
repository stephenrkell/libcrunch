#include <stdio.h>
#include <assert.h>

long f(long int i, void *arg)
{
	return i + (unsigned long long) arg;
}

int main(void)
{
	void *fake = &f;
	
	/* Want a cast that will NOT exercise check_args but will hit the case of 
	 * __is_a_function_refining.
	 *
	 * In this case we're creating function ptr whose use will do
	 *  - implicit cast of arg1 from void* to long int, which is okay;
	 *  - implicit cast of arg2 from long* to void*, which is okay;
	 *  - implicit cast of long return value to void*, which is okay. */
	void *(*fake_fun)(void *, long *) = (void *(*)(void*, long *)) fake;
	
	long l = 42;
	
	void *recovered = fake_fun(&fake, &l);

	printf("It says: %p\n", recovered);

	return 0;
}
