#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

struct B
{
	unsigned *pis[2];
};

struct A
{
	struct B bs[4];
};

void do_it(void* p)
{
	unsigned **recovered = (unsigned**) ((uintptr_t) p - sizeof (unsigned **));
	struct B *blah = (struct B *) recovered;
	printf("We got: (recovered) %u, (indexed) %u\n", *blah->pis[0], *recovered[1]);
}

int main(void)
{
	/* Here we are using a pattern (a bit like that in milc from SPEC CPU2006)
	 * where we do some casts with funky address arithmetic.
	 *
	 * What should happen:
	 * 
	 * When we do the cast to char*, we prefill the cache with the *old* type
	 * info. We have this from two sources: the static type of the cast-from
	 * pointer, and its bounds information.
	 *
	 * This allocation may already be in the cache, of course. We use an out-
	 * -of-line helper, mostly dual to __fetch_bounds_from_cache.
	 *
	 * Short summary: we try not to lose information.
	 */
	
	unsigned first = 1;
	unsigned second = 2;
	unsigned third = 3;
	unsigned fourth = 4;
	
	struct A a = { .bs = { [0] = (struct B) { .pis = { [0] = &first, [1] = &second } },
	                       [1] = (struct B) { .pis = { [0] = &third, [1] = &fourth } } } };
	
	/* NOTE: I initially had the "generic pointer" be char*.
	 * This causes two cache-missing queries.
	 * One is to get the bounds of the allocation.
	 * The other is to do the same... because the stack is not a cacheable allocator. BAH.
	 */
	do_it((void*) &a.bs[0].pis[1]);
	do_it((void*) &a.bs[1].pis[1]);
	return 0;
}
