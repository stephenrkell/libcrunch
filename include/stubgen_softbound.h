#ifdef LIBCRUNCH_EMULATE_SOFTBOUND

#define do_wrapper_init(name) \
  _Bool __caller_is_inst = __tweak_argument_bounds_cookie(__wrap_## name); \
  int argi = 0;

#define round_down_size(sz, round_to) \
    ((round_to) * ((sz) / (round_to)))
extern unsigned long __liballocs_get_alloc_size(const void *obj);
/* It's wrong to use the size_arg, because e.g. for calloc-alikes, 
 * that doesn't tell us how *many* of the object are being created.
 * So ask liballocs how big it is. Round that to a multiple of the
 * size arg. */
#define do_ret_p(name) \
  unsigned long result_sz = __liballocs_get_alloc_size(real_retval); \
  unsigned long result_sz_rounded = round_down_size(result_sz, size_arg_ ## name); \
  __libcrunch_bounds_t result_bounds = __make_bounds(real_retval, \
      (char*) real_retval + result_sz_rounded ); \
  __push_local_result_bounds(__caller_is_inst, result_bounds);

#endif
