#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Disable optimization to prevent compiler from inlining our 
 * xmalloc call. This doesn't usually happen for malloc wrappers; 
 * only because we're in the same file (and linking an executable?). 
 * ALSO, note that --wrap won't work for us; recall that --wrap will
 * 
 * - change an undefined symbol X to __wrap_X; 
 * - change an undefined symbol __real_X to X. 
 *
 * It does nothing for defined symbols! Which is what we really want to
 * have here, i.e. having the wrapper and its client in the same file. 
 * So to hack around this, we leave xmalloc undefined, but define
 * __my_xmalloc here, then use a magic --defsym option (in mk.inc) to fix 
 * up the xmalloc-> reference later. NOTE: calling it __real_xmalloc 
 * doesn't work, because the --defsym,xmalloc=__real_xmalloc will
 * bizarrely cause xmalloc to take the value zero. */

void *(__attribute__((optimize("O0"))) __my_xmalloc)(size_t s)
{
	return malloc(s);
}

void *xmalloc(size_t s);

int  (__attribute__((optimize("O0"))) main)(void)
{
	int *blah = (int *) xmalloc(200 * sizeof (int));
	for (int i = 0; i < 200; ++i) blah[i] = 42;
	
	void *fake = blah;

	int *recovered = (int *) fake;

	printf("It says: %d\n", recovered[0]);

	free(blah);
	
	return 0;
}
