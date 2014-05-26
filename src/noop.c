static const int __libcrunch_is_initialized = 1;

void __libcrunch_scan_lazy_typenames(void *blah) {}

int __is_a_internal(const void *obj, const void *r)
{
        return 1;
}

int __like_a_internal(const void *obj, const void *r)
{
        return 1;
}

int __check_args_internal(const void *obj, int nargs, ...)
{
        return 0;
}
