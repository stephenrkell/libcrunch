#include <stdio.h>
#include <assert.h>

static int called(int blah)
{
	void *fake_blah = &blah;
	return *(int *)fake_blah;
}

int main()
{
	int b = 42;
	int got = called(b);
	assert(got == b);

	return 0;
}
