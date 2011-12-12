#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>

int main(void)
{
	void *alloc1 = malloc(sizeof (struct stat));
	void *alloc2 = malloc(sizeof (void *));
	void *alloc3 = malloc(42);

	return errno;
}
