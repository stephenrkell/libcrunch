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
	/* We should now get the bounds of the whole allocation. BUT WE DON'T. What's going on? */
	char *cp = (char *) cpp; // loop goes backwards but this has absolutely no significance
	for (int j = NBLOCK - 1; j > 0; --j)     // This loop is threading a free list through *all 50*
	{                                        // elements in the calloc'd array, including the one
		cp += elemsize;                      // we will shortly unlink and issue. <-- pre-increment
		*cpp = cp;                           // cp to the next entry in the block, then point *cpp
		cpp = (char **)cp; /* succeeds! ^^ */// to that next element, then advance cpp to point at
	}                                        // that element for the next run
	*cpp = NULL;                             // set the next-in-freelist ptr to NULL, in last elt
	
	// unlink one and return it... the next-free pointer is still intact, just trashable by caller
	cp = (char *)freelist;
	freelist = (char **)(*freelist);         // update freelist (remember it's static)
	return cp; // (a real version of this allocator would check for a non-empty freelist *first*)
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
