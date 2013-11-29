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
	const char blah[] = "Hello, programmer.";
	const char padding2[42];
	twiddle(padding1);
	twiddle(padding2);
	
	void *fake_start = &blah;

	const char *recovered_start = (const char *) fake_start;

	printf("%s\n", recovered_start);
	
	// also try from the middle
	void *fake_middle = &blah[7];
	const char *recovered_middle = (const char *) fake_middle;
	assert(0 == strcmp(recovered_middle, "programmer."));

	return 0;
}
