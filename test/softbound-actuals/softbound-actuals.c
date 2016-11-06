#include <stdio.h>

struct ptrs
{
	char *p1;
	char *p2;
	char *ps[3];
};

struct many_ptrs
{
	struct ptrs many[5];
};

static void f(char *p, struct ptrs x1, struct many_ptrs xx)
{
	printf("It says: %c%c%c%c%c%c%c%c%c%c%c\n", 
		p[1],
		x1.p1[2],
		x1.p2[3],
		x1.ps[0][0],
		x1.ps[1][1],
		x1.ps[2][2],
		xx.many[0].p1[2],
		xx.many[1].p2[2],
		xx.many[2].ps[0][3],
		xx.many[3].ps[1][0],
		xx.many[4].ps[2][1]
	);
	
	++p;
	++x1.p1;
	++x1.p2;
	++x1.ps[0];
	++x1.ps[1];
	++x1.ps[2];
	++xx.many[0].p1;
	++xx.many[1].p2;
	++xx.many[2].ps[0];
	++xx.many[3].ps[1];
	++xx.many[4].ps[2];
	
	printf("Now it says: %c%c%c%c%c%c%c%c%c%c%c\n", 
		p[1],
		x1.p1[2],
		x1.p2[3],
		x1.ps[0][0],
		x1.ps[1][1],
		x1.ps[2][2],
		xx.many[0].p1[2],
		xx.many[1].p2[2],
		xx.many[2].ps[0][3],
		xx.many[3].ps[1][0],
		xx.many[4].ps[2][1]
	);
}

char msg1[] = "Hello, world!";
char msg2[] = "Hello, sailor!";
char msg3[] = "Fool!";

int main(void)
{
	f(
		msg2, 
		(struct ptrs)      { msg1, msg2, { msg3, msg3, msg3 } }, 
		(struct many_ptrs) {
			(struct ptrs) { msg1, msg2, { msg3, msg2, msg1 } }, 
			(struct ptrs) { msg2, msg3, { msg2, msg3, msg2 } }, 
			(struct ptrs) { msg3, msg1, { msg1, msg1, msg3 } }, 
			(struct ptrs) { msg1, msg2, { msg3, msg2, msg1 } }, 
			(struct ptrs) { msg2, msg3, { msg2, msg3, msg2 } }
		}
	);
	return 0;
}

/* 
struct {
	char *p;
} local_struct;

local_struct.p = &as[-1]
		
FIXME: local arrays of pointers
 */
