#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Disable optimization to prevent compiler from un-indirecting 
 * our indirect calls. */
 
struct S
{
	void *(*fn)(size_t);
} s;

void *(*fs[2])(size_t);

int (__attribute__((optimize("O0"))) main)()
{
	void *(*fn)(size_t) = &malloc;
	
	int *blah = (int *) fn(200 * sizeof (int));
	
	for (int i = 0; i < 200; ++i) blah[i] = 42;
	
	void *fake = blah;

	int *recovered = (int *) fake;

	printf("It says: %d\n", recovered[0]);

	free(blah);

	void *(**fn2)(size_t) = &fn;
	
	blah = (int *) (*fn2)(200 * sizeof (int));
	
	fake = blah;
	recovered = (int *) fake;
	free(blah);

	void *(***fn3)(size_t) = &fn2;
	
	blah = (int *) (***fn3)(200 * sizeof (int));
	fake = blah;
	recovered = (int *) fake;
	free(blah);
	
	s.fn = &malloc;
	blah = s.fn(200 * sizeof (int));
	fake = blah;
	recovered = (int *) fake;
	free(blah);
	
	fs[1] = &malloc;
	blah = fs[1](200 * sizeof (int));
	fake = blah;
	recovered = (int *) fake;
	free(blah);
	
	return 0;
}
