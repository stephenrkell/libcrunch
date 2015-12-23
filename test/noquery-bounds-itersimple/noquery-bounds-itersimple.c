#include <stdio.h>

int main(void)
{
	/* Test that we can iterate through a large array while only doing 
	 * a *single* query. */
	
	int is[100] = {
		[0] = 100,
		[99] = 1
	};
	int *pi = &is[0];
	
	while (pi != &is[100])
	{
		if (*pi != 0)
		{
			printf("Found a nonzero: %d at %d\n", *pi, (int) (pi - &is[0]));
		}
		
		++pi;
	}
	
	/* How many queries should the above do? 
	 * We get the bounds of pi locally, because it's from a local array 
	 * (address-takenness is irrelevant). 
	 * We should locally check and trap the creation of &is[100]. 
	 * In short, I think no queries at all.
	 * Even the __check_local_bounds should be optimised away by the compiler,
	 * I reckon.
	 */
	
	return 0;
}
