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
	
	char *a = &as[41];
	printf("Got a: %p\n", a);

	char *b = &as[1];
	printf("Got b: %p\n", b);

	char *c = &as[a - b]; // difference 40
	printf("Got c: %p\n", c);

	char *d = &bs[as[1]]; // value 'b' a.k.a. 98
	printf("Got d: %p\n", d);
	
	char **e = &cs[0].plugh;
	printf("Got e: %p\n", e);

	return 0;
}

/* 
struct {
	char *p;
} local_struct;

local_struct.p = &as[-1]
		
FIXME: local arrays of pointers
 */
