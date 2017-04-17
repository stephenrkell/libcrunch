/* These wrappers are only sane if void* carry bounds. 
 * Otherwise there are no pointers to propagate. */

#ifdef LIBCRUNCH_VOID_POINTERS_HAVE_BOUNDS

#define do_caller_wrapper_init(name) \
  _Bool __caller_is_inst = __tweak_argument_bounds_cookie(__wrap_## name); \
  int argi = 0; \
  int argpeek_off = 0; \
  int realarg_off = 0; \
  unsigned long *saved_bsp = __bounds_sp; \

#define round_down_size(sz, round_to) \
    ((round_to) * ((sz) / (round_to)))
extern unsigned long __liballocs_get_alloc_size(const void *obj);
/* It's wrong to use the size_arg, because e.g. for calloc-alikes, 
 * that doesn't tell us how *many* of the object are being created.
 * So ask liballocs how big it is. Round that to a multiple of the
 * size arg. */
#define do_ret_p(name) \
  __bounds_sp = saved_bsp; \
  unsigned long result_sz = __liballocs_get_alloc_size(real_retval); \
  unsigned long result_sz_rounded = round_down_size(result_sz, size_arg_ ## name); \
  __libcrunch_bounds_t result_bounds = __make_bounds(real_retval, \
      (char*) real_retval + result_sz_rounded ); \
  __push_local_result_bounds(__caller_is_inst, result_bounds);

#define do_ret_z(name) \
  __bounds_sp = saved_bsp;
  
#define do_ret_i(name) \
  __bounds_sp = saved_bsp; \
  
#define do_ret_void(name) \
  __bounds_sp = saved_bsp;

#define do_arginit_p(name) \
  __libcrunch_bounds_t argbound_ ## name = __peek_argument_bounds(__caller_is_inst, argpeek_off++, \
      name, #name);

#define do_arginit_P(name) do_arginit_p(name)

/* For any pointer argument, we have to forward its bounds. */
#define pre_realarg_p(argname) \
	__push_local_argument_bounds(argbound_ ## argname);

#define pre_realarg_P(argname) \
	pre_realarg_p(argname)

#define pre_realcall(callee, ...) \
	__push_argument_bounds_cookie(callee);

#endif
