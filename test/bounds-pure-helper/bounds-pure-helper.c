#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

static const char mybuf[] = "The quick brown fox jumped over the lazy dog";
/* FIXME: if we use just a string literal here, we don't get bounds
 * because we don't get static alloc records string lits in rodata. */

char *my_pure(const char*) __attribute__((noinline,__const__));
char *my_pure(const char *needle)
{
	static _Bool ran_once;
	assert(!ran_once);
	ran_once = 1;
	return strstr(mybuf, needle);
}

/* We must be able to call a pure function *twice*, 
 * say strstr, and get valid bounds for what it returns
 * even the second time, but only make one call.
 * Naively the shadow stack breaks this because 
 * the second call will appear uninstrumented. */
int ( __attribute__((optimize("O2"))) main)(void)
{
	char *needle1 = my_pure("quick");
	printf("Needle second char: %c\n", needle1[1]);
	char *needle2 = my_pure("quick");
	printf("Needle second char: %c\n", needle2[1]);
	
	return 0;
}
