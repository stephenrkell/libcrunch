/* This is based on sphinx3's ckd_alloc.c / __mymalloc__,
 * which creates small object pools
 * and links the free ones together in a free list.
 * This linking disrespects the type of the object,
 * by blatting the free-list pointers into the first word of
 * each object. */

#include <stdlib.h>
#include <stdio.h>

void *alloc(size_t elemsize)
{
	static char **freelist;
#define NBLOCK 50
	/* This cast happens to be valid for our caller, because the object's first argument
	 * is a void* and char** is a generic pointer pointer. However, we get the bounds
	 * [cpp, cpp+8bytes).*/
	char **cpp = freelist = (char **) calloc(NBLOCK, elemsize);
	/* We should now get the bounds of the whole allocation. */
	char *cp = (char *) cpp;
	for (int j = NBLOCK - 1; j > 0; --j)
	{
		cp += elemsize;
		*cpp = cp;
		cpp = (char **)cp;
	}
	*cpp = NULL;
	
	// unlink one and return it
	cp = (char *)freelist;
	freelist = (char **)(*freelist);
	return cp;
}


struct myelem
{
	void *blah;
	int x;
};

int main(void)
{
	struct myelem *e = alloc(sizeof (struct myelem));
	e->blah = NULL;
	e->x = 42;
	printf("At %p, blah is %p and x is %d\n", e, e->blah, e->x);
	return 0;
}
