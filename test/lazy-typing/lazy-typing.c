#include <stdio.h>
#include <assert.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "libcrunch.h"

void *test_void_ptr;
struct sockaddr test_sockaddr;

int main(void)
{
	struct foo {
		int bar;
	} **blah = malloc(sizeof (void*));
	
	assert(__libcrunch_is_initialized);
	
	//struct sockaddr_in *p_mysock = malloc(sizeof (struct sockaddr));
	/* FIXME: reinstate this once we have arbitrary type abstraction. */
	struct sockaddr_in *p_mysock = malloc(sizeof (struct sockaddr_in));
	
	fprintf(stderr, "Allocated a sockaddr_in at %p\n", p_mysock);

	return 0;
}
