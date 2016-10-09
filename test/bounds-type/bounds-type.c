#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

extern unsigned long int __libcrunch_fetch_bounds_missed_cache;

/* This test case is showing that bounds do affect types, and correctly.
 * We also test performance properties: if the is-a cache is doing its job,
 * a malloc should not cause a fetch-bounds cache miss. This only holds with
 * trumptr also enabled. */
 
struct blah
{
	int x[2];
	float y;
};

int main(void)
{
	void *alloc = calloc(42, sizeof (struct blah));

	struct blah *p_zs = (struct blah *) alloc;
	assert(__libcrunch_fetch_bounds_missed_cache == 0);
	/* After this we're allowed to miss the cache, but *only* for 
	 * char*! And perhaps GPP types that are not always checked. 
	 * The other target types should be cached from trumptr, as before. */
	
	printf("An integer: %d\n", *((p_zs->x + 42) - 42)); // #3: non-error: via out-of-bounds intermediate
	
	printf("A float: %f\n", ((struct blah *) p_zs->x)->y); // #4: non-error: after bounds-widening cast
	
	printf("An integer: %d\n", *(int*)(intptr_t)p_zs->x); // #5: non-error: via integer
	
	char *a_y = (char*) &p_zs[41].y;
	printf("A pointer ranging over the whole allocation: %p\n", a_y);
	
	/* Coup de grace: use char* arithmetic to recover a float* starting from an int*,
	 * all without complaint from libcrunch. */
	printf("An integer: %d\n", *(int *)(
		a_y
			- offsetof(struct blah, y)
			- 41 * sizeof (struct blah)
		));

	return 0;
}
