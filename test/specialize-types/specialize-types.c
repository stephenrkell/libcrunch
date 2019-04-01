#include <stdlib.h>

static void copy_ptr(void **dst, int **src)
{
	*dst = *src;
}

int main(void)
{
	// allocate an array of void*
	void **p = malloc(8 * sizeof (void*));
	// make an int** point to it -- specializing it to int*
	// (specialization by pointing-something-at)
	int **pi = p;
	//assert(__liballocs_get_type(pi) == &__uniqtype____PTR_int);
	// use a generic function to manipulate the int* as if they were void*
	// (specialization by storing-into... but, hmm, storing into a void** is okay??)
	void **buf = malloc(sizeof (void*));
	copy_ptr(buf, pi);
	// specialization by casting
	int **x = (int**) malloc(2 * sizeof (void*));
	printf("It was: %p\n", x);
	return 0;
}
