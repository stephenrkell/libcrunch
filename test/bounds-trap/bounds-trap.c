#include <stdio.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following.
	 * They are all made to use one-past pointers. */
	
	char as[42] = "abcdefghijklmnopqrstuvwxyz0123456789ABCDE";
	char bs[100] = "qwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzx";
	struct {
		int xyzzy;
		char *plugh;
	} cs[2] = { { 101, "a" }, { 102, "b" } };
	
	char *a = &as[42];
	printf("Got a - 1: %p\n", a - 1);

	char *b = &as[42];
	printf("Got b - 41: %p\n", b - 41);

	char *d = &bs[as[1] + 2] - 2; // value 'b' a.k.a. 98
	printf("Got d: %p\n", d);

	return 0;
}
