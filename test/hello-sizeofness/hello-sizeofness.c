#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Here we do an allocation which can only be correctly classified
 * by a sizeofness-propagating malloc scraper.*/

static void *alloc(void)
{
	size_t size_to_allocate = sizeof (long);
	return malloc(size_to_allocate);
}

int main()
{
	void *my_obj = alloc();
	*(long *)my_obj = 42;
	printf("My obj is %ld\n", *(long *)my_obj);

	return 0;
}
