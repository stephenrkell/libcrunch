#define _GNU_SOURCE
#include <stdio.h>
#include <assert.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <dlfcn.h>

#include "libcrunch.h"
#include "libcrunch_cil_inlines.h" // for __like_aU

void *test_void_ptr;
struct sockaddr test_sockaddr;

int main(void)
{
	struct sockaddr_in *p_mysock = malloc(sizeof (struct sockaddr));
	
	fprintf(stderr, "Allocated a sockaddr_in at %p\n", p_mysock);
	
	const void *sockaddr_type = dlsym(RTLD_DEFAULT, "__uniqtype__sockaddr");
	assert(sockaddr_type);
	
	assert(__like_aU(p_mysock, sockaddr_type));

	return 0;
}
