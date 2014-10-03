static const int __libcrunch_is_initialized = 1;
unsigned long int __libcrunch_begun = 0;
unsigned long int __libcrunch_failed = 0;
unsigned long int __libcrunch_aborted_typestr = 0;

void __libcrunch_scan_lazy_typenames(void *blah) {}

int __libcrunch_check_init(void)
{
        return 0;
}

int __libcrunch_global_init(void)
{
        return 0;
}

int __is_a_internal(const void *obj, const void *r)
{
        return 1;
}

int __like_a_internal(const void *obj, const void *r)
{
        return 1;
}

int __named_a_internal(const void *obj, const void *r)
{
        return 1;
}

int __check_args_internal(const void *obj, int nargs, ...)
{
        return 1;
}

int __is_a_function_refining_internal(const void *obj, const void *r)
{
        return 1;
}
int __is_a_pointer_of_degree_internal(const void *obj, int d)
{
        return 1;
}
int __can_hold_pointer_internal(const void *obj, const void *target)
{
        return 1;
}
