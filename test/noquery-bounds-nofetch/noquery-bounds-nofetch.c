#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct B
{
    int a[4];
    unsigned *pis[2];
    float c[1];
	int y;
};

struct A
{
    struct B bs[4];
    int x;
    struct B another;
};

static unsigned blah = 42;

int main(void)
{
	/* We should be able to index into these arrays
	 * using a non-constant index expression
	 * and still not generate any bounds fetches,
	 * because we're indexing inside a local object. 
	 * Instead, we'll generate checks that the expression
	 * is within the local bounds. */
	
	struct A as[3] = {
		[0] = { /* an A starts here */ 
			{ [0] = { /* a B starts here */
					/* a */ { [0] = 0 }, 
					/* pis */ { [0] = &blah }, 
					/* c */ { [0] = 0.0 }, 
					/* y */ 0 
				}
			}, 
			/* x */ 1, 
			/* another B starts here */
			{ { [0] = 0 }, { [0] = NULL }, { [0] = 0.0 }, 0 }
		},
		[1] = { /* an A starts here */ 
			{ [3] = { /* a B starts here */
					/* a */ { [0] = 0 }, 
					/* pis */ { [0] = &blah }, 
					/* c */ { [0] = 0.0 }, 
					/* y */ 1 
				}
			}, 
			/* x */ 0, 
			/* another B starts here */
			{ { [0] = 0 }, { [0] = NULL }, { [0] = 0.0 }, 0 }
		},
		[2] = { /* an A starts here */ 
			{ [3] = { /* a B starts here */
					/* a */ { [0] = 0 }, 
					/* pis */ { [0] = &blah }, 
					/* c */ { [0] = 0.0 }, 
					/* y */ 0 
				}
			}, 
			/* x */ 0, 
			/* another B starts here */
			{ { [0] = 0 }, { [0] = &blah }, { [0] = 0.0 }, 0 }
		}
	};
	/* GAH. Don't do memset, because it makes the storage address-taken. 
	 * FIXME: might want to make an exception for memset in the 
	 * address-taken analysis done by CIL. */
	// memset(as, 0, sizeof as);
	
	/* In total, this local array
	 * contains
	 * 
	 *  3 * (4 + 1) * 2     = 30
	 * 
	 * bounds objects. */
	printf("Some numbers from in-bounds accesses: %d %d %d\n",
		as[(int) (drand48() * 2)].x,
		as[1].bs[(int) (drand48() * 2)].y,
		as[2].bs[3].a[(int) (drand48() * 4)]);
	printf("Some pointers from in-bounds accesses: %p %p %p\n",
		as[0].bs[(int) (drand48() * 2)].pis[0],
		as[1].bs[3].pis[(int) (drand48() * 2)],
		as[2].another.pis[(int) (drand48() * 2)]);
	
	return 0;
}
