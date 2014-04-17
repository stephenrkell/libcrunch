#include <stdlib.h>

struct Foo
{
	int a;
	float b;
};

/* GRR: we have to disable optimisation because gcc will turn
 * the malloc call into a jmp, and that means the *caller* will
 * show up as the allocation site. We could make create_foo an
 * allocation function, but that is displeasing in other ways.
 */
void *create_foo(void) __attribute__((optimize("O0")));
void *create_foo(void)
{
	return malloc(sizeof(struct Foo));
}
