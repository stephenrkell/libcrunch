#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stddef.h>

int main(void)
{
	// heap-allocate some libc data types
	struct sockaddr *my_sockaddr = (struct sockaddr*) malloc(sizeof (struct sockaddr));
	struct stat *my_stat = (struct stat *) malloc(sizeof (struct stat));
	char *stat_as_char = (char *) my_stat;
	
	unsigned long spin_length = 0UL;
	char *envvar = getenv("SPIN_LENGTH");
	spin_length = atoi(envvar ?: "0");
	if (spin_length == 0UL) spin_length = 1000;
	
	/* Base type HACKs... */
	struct bases {
	char s; unsigned char us; signed char sc; int t; unsigned int ut; long int v; long unsigned int uv;
	long long int w; long long unsigned int uw;
	};
	struct bases *my_bases = malloc(sizeof (struct bases));
	
	/* Now go round in a loop */
	for (int i = 0; i < 1000; ++i)
	{
		/* Do a pointer cast */
		if (i % 3 == 0)
		{
			/* Do an incorrect one */
			struct stat *my_ptr = (struct stat *) my_sockaddr;
		}
		else if (i % 3 == 1)
		{
			/* Do a fast correct one */
			ino_t *my_ptr = (ino_t *) (stat_as_char + offsetof(struct stat, st_ino));
		}
		else if (i % 3 == 2)
		{
			/* Do a slow correct one */
			time_t *my_time = (time_t *) (stat_as_char + offsetof(struct stat, st_ctime));
		}
		
		/* Spin for the number of instructions that we were asked to
		 * -- HACK: guess that our loop runs two instrs/cycle */
		for (unsigned long u = 0UL; u < spin_length/2; ++u);
	}
}
