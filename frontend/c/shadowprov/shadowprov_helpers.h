/* FIXME: if I don't do the definition in two stages like this,
 * starting with the opaque declaration, CIL forks into two struct
 * defs, one of them alpha-renamed to shadow_byte___0... WHY? */
struct shadow_byte;
typedef struct shadow_byte __shadow_t;
struct shadow_byte { char byte; };
typedef char __raw_shadow_t;
/* This must be a type to which a cast from any shadowed primitive
 * succeeds and doesn't create a new shadow: it might discard one
 * or leave it unchanged. */
typedef unsigned long __shadowed_value_t;
extern __thread unsigned long */*volatile*/ __shadow_sp;
#define NULL_SHADOW ((char)0)
#define SPECIAL_SHADOW ((char)255) /* FIXME: do I need this? */

/* FIXME: in places where we memcpy bytes, we assume little-endianness. */

/* GNU extensions "&&label" and gnu_inline might be better, but that sadly
 * doesn't work on my current gcc (4.9.2) because it mistakenly thinks that 
 * "&&mylabel" is a local variable, then does bogus UB optimisations on the
 * result. SIGH. Use an arch-specific sequence instead. DOUBLE SIGH: if we
 * write "callq 0" it gets relocated, so use .byte. */
extern inline void * (__attribute__((always_inline,gnu_inline,used)) __get_pc)(void)
{
	void *addr;
	__asm__ volatile (".byte 0xe8   # callq \n\
	                   .byte 0x0    # 0 \n\
	                   .byte 0x0    # \n\
	                   .byte 0x0    # \n\
	                   .byte 0x0    # \n\
	                   pop %0" : "=r"(addr));
	return addr;
}

extern inline __shadow_t *(__attribute__((always_inline,gnu_inline,pure,__const__))
		__shadow_stored_addr)(const void *addr)
{
	return (__shadow_t *)((0x70ul << 0x28) ^ (unsigned long) addr);
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,pure,__const__))
		__make_invalid_shadow)(__shadowed_value_t v)
{
	return (__shadow_t) { (char) 0 };
}

extern inline void
(__attribute__((always_inline,gnu_inline,used,nonnull(1))) __store_shadow_nonlocal)
(void *dest, __shadowed_value_t v, __shadow_t shadow,
	void *arg)
{
	unsigned long dest_addr __attribute__((unused)) = (unsigned long) dest;
	unsigned long shadow_stored_addr __attribute__((unused))
	 = (unsigned long) __shadow_stored_addr(dest);
	
	*(__raw_shadow_t *) shadow_stored_addr = (__raw_shadow_t) shadow.byte;
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,used,pure))
	__fetch_shadow_inl)(__shadowed_value_t v, const void *loaded_from, void *t)
{
	if (loaded_from)
	{
		return (__shadow_t) { *(__raw_shadow_t *) __shadow_stored_addr(loaded_from) };
	}
	return __make_invalid_shadow(v);
}

extern inline void *(__attribute__((always_inline,gnu_inline,used,malloc)) __alloc_shadow_stack_space)
	(unsigned long nbytes)
{
	/* This always returns an address aligned to the size of
	 * the argument or 8 bytes, whichever is smaller. 
	 * NOTE that we never use the mixed-alignment support;
	 * it's essential that the 8-byte cookie directly abuts the
	 * shadow values, otherwise our offset-based peek calculation
	 * will start reading padding, not shadow bytes. So we assert
	 * that the alignment delta equals zero. */
	unsigned long align = (nbytes < 8) ? nbytes : 8;
	/* First align the sp to the alignment we need. */
	//warnx("Allocating (bsp %p) shadow stack space: %d bytes, %d-aligned", __shadow_sp, nbytes, align);
	unsigned alignment_delta = ((unsigned long) __shadow_sp) % align;
	if (!(alignment_delta == 0)) abort();
	__shadow_sp = (unsigned long *) ((char*) __shadow_sp - alignment_delta);
	/* Then allocate. */
	__shadow_sp = (unsigned long *) ((char*) __shadow_sp - 
		((nbytes % align == 0) ? nbytes : align * (1 + (nbytes / align))));
	//warnx("After shadow stack allocation, bsp is %p", __shadow_sp);
	return __shadow_sp;
}

/* The shadow stack works as follows.
 * FIXME: paste/translate comment from libcrunch_cil_inlines.h
 */

/* We need to be really careful about strict aliasing rules here. 
 * We should only access the shadow stack with lvalues of type unsigned long.
 * To access shadow, we use these funky macros which call memcpy.
 * Actually we call __builtin_memcpy because we don't want to hit
 * our funky instrumented/preloaded version. */

#ifndef SHADOW_STACK_TRACE_LEVEL
#define SHADOW_STACK_TRACE_LEVEL 0
#endif

#define PRISHADOW "%ld"
#define PRINT_SHADOW_EXPR(s) (long)*(char*)&s
#define TRACE_SHADOW(lvl, descr, val, args...) (((lvl)<SHADOW_STACK_TRACE_LEVEL) ? (void)0 : warnx("(bsp=%p): shadow " PRISHADOW " " descr, __shadow_sp, PRINT_SHADOW_EXPR(val), ## args ))
#define TRACE_WORD(lvl, descr, w, args...) (((lvl)<SHADOW_STACK_TRACE_LEVEL) ? (void)0 : warnx("(bsp=%p): word %lx " descr, __shadow_sp, w, ## args ))
#define TRACE_STRING(lvl, descr, s, args...) (((lvl)<SHADOW_STACK_TRACE_LEVEL) ? (void)0 : warnx("(bsp=%p): %s " descr, __shadow_sp, s, ## args ))

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,pure,__const__))
		__make_shadow)(__shadowed_value_t arg1, __shadowed_value_t arg2)
{
	/* The two arguments we get are (1) a pointer-sized value denoting some property
	 * of the allocation, and (2) an opaque word-sized value that refines this
	 * to denote the allocation uniquely. It might be zero.
	 * We take the second-from-bottom byte of the pointer and XOR it with the offset. */
	if (!arg1) return (__shadow_t) { NULL_SHADOW };
	__raw_shadow_t s = (char) (1 + ((arg1 >> 8) ^ arg2) % 254);
	TRACE_SHADOW(1, "made fresh from pointer %p", s, (void*) arg1);
	return (__shadow_t) { (char) (1 + ((arg1 >> 8) ^ arg2) % 254) };
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_local_argument_shadow)
	(__shadow_t shadow)
{
	unsigned long tmp = shadow.byte;
	unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
	__builtin_memcpy(b, &tmp, sizeof (tmp));
	TRACE_SHADOW(1, "pushed", shadow);
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_argument_shadow_manifest)
	(__shadowed_value_t v __attribute__((unused)), __shadowed_value_t arg1, __shadowed_value_t arg2)
{
	unsigned long tmp = __make_shadow(arg1, arg2).byte;
	unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
	__builtin_memcpy(b, &tmp, sizeof tmp);
	TRACE_SHADOW(1, "pushed", tmp);
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_argument_shadow_explicit)
	(__shadowed_value_t v __attribute__((unused)), __raw_shadow_t c)
{
	unsigned long tmp = c;
	unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
	__builtin_memcpy(b, &tmp, sizeof tmp);
	TRACE_SHADOW(1, "pushed", tmp);
}

extern inline void (__attribute__((always_inline,gnu_inline,used))
	__fetch_and_push_argument_shadow)( __shadowed_value_t v, const void *loaded_from, void *t)
{
	unsigned long tmp = __fetch_shadow_inl(v, loaded_from, t).byte;
	unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
	__builtin_memcpy(b, &tmp, sizeof tmp);
	TRACE_SHADOW(1, "pushed", tmp);
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_argument_shadow_cookie)
	(const void *callee, const char *callee_name)
{
	unsigned long *c = __alloc_shadow_stack_space(sizeof (unsigned long));
	__builtin_memcpy(c, &callee, sizeof callee);
	TRACE_STRING(1, "about to call (cookie %p)", callee_name, callee);
}

extern inline _Bool (__attribute__((always_inline,gnu_inline,used)) __tweak_argument_shadow_cookie)
	(const void *callee)
{
	/* Consume the cookie, by changing it to an address which is unique to the callee 
	 * and is not the valid address of *any* callee. */
	if (0 == __builtin_memcmp(__shadow_sp, &callee, sizeof callee)) 
	{
		TRACE_WORD(1, "matched expected cookie", *__shadow_sp);
		*__shadow_sp |= 0x800000000000ul; 
		return 1;
	}
	else
	{
		TRACE_WORD(0, "shadow cookie did not match (vs %p)", *__shadow_sp, callee);
		return 0;
	}
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,used,pure)) __peek_argument_shadow)
	(_Bool really, unsigned long offset, __shadowed_value_t v, const char *debugstr __attribute__((unused)))
{
	if (really)
	{
		__shadow_t b;
		unsigned long *b_addr = (unsigned long *)(
			(char*) __shadow_sp + sizeof (unsigned long) + (offset * (sizeof (unsigned long)))
		);
		__builtin_memcpy(&b, b_addr, sizeof b);
		TRACE_SHADOW(1, "peeked for argument", b);
		return b;
	}
	TRACE_STRING(0, "no shadow for shadowed arg expr", debugstr);
	return __make_invalid_shadow(v);
}

extern inline void
	(__attribute__((always_inline,gnu_inline,used)) __peek_and_shadow_store_argument_shadow)
	(_Bool really, void *loaded_from, unsigned long offset, __shadowed_value_t v,
		void *t, const char *debugstr __attribute__((unused)))
{
	if (really)
	{
		__shadow_t b = __peek_argument_shadow(really, offset, v, debugstr);
		__store_shadow_nonlocal(loaded_from, v, b, t);
	}
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_local_result_shadow)
	(_Bool really, __shadow_t shadow)
{
	if (really)
	{
		unsigned long tmp = shadow.byte;
		unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
		__builtin_memcpy(b, &tmp, sizeof tmp);
		TRACE_SHADOW(1, "result pushed", shadow);
	}
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_result_shadow_manifest)
	(_Bool really, __shadowed_value_t v __attribute__((unused)), __shadowed_value_t arg1, __shadowed_value_t arg2)
{
	if (really)
	{
		unsigned long *b = __alloc_shadow_stack_space(sizeof (__raw_shadow_t));
		unsigned long tmp = __make_shadow(arg1, arg2).byte;
		__builtin_memcpy(b, &tmp, sizeof tmp);
		TRACE_SHADOW(1, "result pushed", tmp);
	}
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __push_result_shadow_explicit)
	(_Bool really, __shadowed_value_t v __attribute__((unused)), __raw_shadow_t arg)
{
	if (really)
	{
		unsigned long *b = __alloc_shadow_stack_space(sizeof (__raw_shadow_t));
		unsigned long tmp = arg;
		__builtin_memcpy(b, &tmp, sizeof tmp);
		TRACE_SHADOW(1, "result pushed", tmp);
	}
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __fetch_and_push_result_shadow)
	(_Bool really, __shadowed_value_t v, const void *loaded_from, void *t)
{
	if (really)
	{
		unsigned long tmp = __fetch_shadow_inl(v, loaded_from, t).byte;
		unsigned long *b = __alloc_shadow_stack_space(sizeof tmp);
		__builtin_memcpy(b, &tmp, sizeof tmp);
		TRACE_SHADOW(1, "result pushed", b);
	}
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,used,pure)) __peek_result_shadow)
	(_Bool really, unsigned long offset, __shadowed_value_t v, const char *calleestr __attribute__((unused)))
{
	if (really)
	{
		__shadow_t b;
		__builtin_memcpy(&b, (char*) __shadow_sp + (offset * (sizeof (unsigned long))), sizeof (__shadow_t));
		TRACE_SHADOW(1, "peeked as result", b);
		return b;
	}
	TRACE_STRING(0, " : callee returned no shadow (callee address %p)", calleestr, __get_pc());
	return __make_invalid_shadow(v);
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __cleanup_shadow_stack)(void *saved_ptr);
extern inline void (__attribute__((always_inline,gnu_inline,used)) __cleanup_shadow_stack)(void *saved_ptr)
{
	__shadow_sp = (unsigned long *) saved_ptr;
}

extern inline _Bool (__attribute__((always_inline,gnu_inline,used)) __check_deref)(const void *addr, __shadow_t s);
extern inline _Bool (__attribute__((always_inline,gnu_inline,used)) __check_deref)(const void *addr, __shadow_t s)
{
	return 1;
}
