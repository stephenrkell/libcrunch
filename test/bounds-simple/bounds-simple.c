#include <stdio.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following. */
	
	char as[42] = "abcdefghijklmnopqrstuvwxyz0123456789ABCDE";
	char bs[100] = "qwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzx";
	struct {
		int xyzzy;
		char *plugh;
	} cs[2] = { { 101, "a" }, { 102, "b" } };
	
	char *a = &as[43];
	printf("Got %p\n", a);

	char *b = &as[-1];
	printf("Got %p\n", b);

	char *c = &as[a - b];
	printf("Got %p\n", c);

	char *d = &as[as[10]];
	printf("Got %p\n", d);
	
	char **e = &cs[0].plugh;
	printf("Got %p\n", e);

	return 0;
}

/* 
struct {
	char *p;
} local_struct;

local_struct.p = &as[-1]
		
FIXME: local arrays of pointers
 */
