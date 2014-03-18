#include <stdio.h>

struct r
{
	union s
	{
		struct t
		{
			union u
			{
				int v;
				float w;
			} u;
			int x;
		} t;
		union y
		{
			double z;
			int a;
		} y;
	} s;
} r;

void my_bzero(void *ptr, size_t s)
{
	for (size_t i = 0; i < s; ++i)
	{
		((char*) ptr)[i] = 0;
	}
}

int main(void)
{
	void *p_v1 = &r.s.t.u.v;
	int v1 = *(int*) p_v1;

	void *p_v2 = &r.s.t.u.w;
	float v2 = *(float*) p_v2;

	void *p_v3 = &r.s.t.u.w;
	float v3 = *(float*) p_v3;
	
	void *p_v4 =  &r.s.y.z;
	double v4 = *(double*) p_v4;

	void *p_v5 =  &r.s.y.a;
	int v5 = *(int*) p_v5;

	printf("Success! %d %f %f %f %i\n", v1, v2, v3, (float) v4, v5);
	
	return 0;
}
