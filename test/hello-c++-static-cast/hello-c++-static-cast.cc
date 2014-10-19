#include <iostream>

struct blah
{
	const char *b;
} s = { "Hello, world!" };

int main(void)
{
	void *p = &s;
	// HACK: temporarily disabled this test case
	std::cout << static_cast<blah *>(p)->b << std::endl;
	return 0;
}
