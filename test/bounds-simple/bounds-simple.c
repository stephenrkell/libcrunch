#include <stdio.h>

int main(void)
{
	/* Test that we check all the pointer derivations in the following. */
	
	char as[42] = "abcdefghijklmnopqrstuvwxyz0123456789ABCDE";
	char bs[100] = "qwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzx";
	
	char *a = &as[43];
	printf("Got %p\n", a);

	return 0;
}
