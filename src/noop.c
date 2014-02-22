#include <stdio.h>

static const int __libcrunch_is_initialized = 1;

int __is_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __like_a_internal(const void *obj, const void *r)
{
	return 1;
}

int __libcrunch_global_init(void)
{
	return 0;
}

const void *__libcrunch_typestr_to_uniqtype(const void *r)
{
	return NULL;
}
