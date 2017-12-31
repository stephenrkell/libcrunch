#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <dlfcn.h>

#include "libcrunch.h"

void *test_void_ptr;
struct sockaddr test_sockaddr;

int main(void)
{
	/* HACK: want to "abstract" arbitrary types, so that we can go back
	 * to sockaddr here -- FIXME FIXME FIXME. */
	struct sockaddr_in *p_mysock = malloc(sizeof (struct sockaddr_in));
	
	fprintf(stderr, "Allocated a sockaddr_in at %p\n", p_mysock);
	
	const void *sockaddr_type = dlsym(RTLD_DEFAULT, "__uniqtype__sockaddr");
	assert(sockaddr_type);
	
	assert(__like_aU(p_mysock, sockaddr_type));

	return 0;
}
