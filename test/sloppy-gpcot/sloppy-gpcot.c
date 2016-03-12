#include <stdio.h>

struct ObjA
{
	int *hdr;
	void **other;
};

struct ObjB
{
	float *hdr;
	int **other;
	long more;
};

struct View
{
	void *hdr;
	void **other;
};

int *x = NULL;
int *y = NULL;

int main(void)
{
	struct ObjA o1;
	struct ObjB o2;
	
	((struct View *) &o1)->other = &x;
	((struct View *) &o2)->other = &y;
	
	return 0;
}
