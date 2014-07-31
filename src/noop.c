static const int __libcrunch_is_initialized = 1;

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
