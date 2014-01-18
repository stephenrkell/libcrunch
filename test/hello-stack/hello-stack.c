#include <stdio.h>
#include <assert.h>

int is_a(void *obj, const char *typestr);

// this overridable function ensures we don't get our padding optimised out
void twiddle(char *blah)
{
	
}

int main()
{
	const char padding1[42];
	int blah[] = { 42, 42, 42 };
	const char padding2[42];
	twiddle(padding1);
	twiddle(padding2);
	
	void *fake_start = &blah;

	int *recovered_start = (int *) fake_start;

	printf("It says: %d\n", recovered_start[0]);
	
	// also try from the middle
	void *fake_middle = &blah[2];
	int *recovered_middle = (int *) fake_middle;
	assert(*recovered_middle == 42);

	return 0;
}
