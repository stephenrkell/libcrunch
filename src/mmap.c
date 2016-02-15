#include "systrap.h"
#include "do-syscall.h"

void __liballocs_nudge_mmap(void **p_addr, size_t *p_length, int *p_prot, int *p_flags,
                  int *p_fd, off_t *p_offset, const void *caller);

void mmap_replacement(struct generic_syscall *s, post_handler *post) __attribute__((visibility("hidden")));
void mmap_replacement(struct generic_syscall *s, post_handler *post)
{
	/* Unpack the mmap arguments. */
	void *addr = (void*) s->args[0];
	size_t length = s->args[1];
	int prot = s->args[2];
	int flags = s->args[3];
	int fd = s->args[4];
	off_t offset = s->args[5];
	
	/* Nudge them. */
	__liballocs_nudge_mmap(&addr, &length, &prot, &flags, &fd, &offset, s->saved_context->pretcode);
	
	/* Re-pack them. */
	s->args[0] = (long int) addr;
	s->args[1] = length;
	s->args[2] = prot;
	s->args[3] = flags;
	s->args[4] = fd;
	s->args[5] = offset;
	
	/* Do the call. */
	long int ret = do_syscall6(s);
	
	/* Do the post-handling. */
	post(s, ret);
	
	/* We need to do our own resumption also. */
	resume_from_sigframe(ret, s->saved_context, /* HACK */ 2);
}
