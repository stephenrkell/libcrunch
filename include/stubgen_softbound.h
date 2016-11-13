#ifdef LIBCRUNCH_EMULATE_SOFTBOUND

#define do_wrapper_init(name) \
  _Bool __caller_is_inst = __tweak_argument_bounds_cookie(__wrap_## name); \
  int argi = 0;

#define do_ret_p(name) \
  __libcrunch_bounds_t result_bounds = __make_bounds(real_retval, (char*) real_retval + size_arg_ ## name); \
  __push_local_result_bounds(__caller_is_inst, result_bounds);

#endif
