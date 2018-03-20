#include <stdio.h>

struct ObjA
{
	int *hdr;
	struct
	{
		void **other;
	} rest;
};

struct ObjB
{
	float *hdr;
	struct
	{
		int **other;
	} rest;
	long more;
};

struct View // even a good view can be used badly...
{
	void *hdr;
	struct
	{
		void **other;
	} rest;
};
struct BadView1
{
	void **hdr;
	int **other;
};

int *x = NULL;
int *y = NULL;
float *z = NULL;

int main(void)
{
	struct ObjA o1;
	struct ObjB o2;
	
	((struct BadView1 *) &o1)->other = &x;
	((struct BadView1 *) &o2)->other = &y;
	((struct View *) &o1)->rest.other = &z;
	
	return 0;
	
}
