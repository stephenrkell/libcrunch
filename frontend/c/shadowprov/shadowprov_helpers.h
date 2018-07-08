// #define LIBALLOCS_NO_INLCACHE /* to be 'safe' from this experimental optimisation for now */
// #include <liballocs.h>
// don't do this... pull in too much

void *(__attribute__((pure)) __liballocs_get_alloc_base)(const void *obj); // better

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
extern __thread unsigned long *volatile __shadow_sp;
#define NULL_SHADOW ((char)0)
#define SPECIAL_SHADOW ((char)255) /* FIXME: do I need this? */

void __assert_fail(const char *, const char *, unsigned int, const char *);
#ifndef assert
#define assert(cond) \
	if (!(cond)) __assert_fail(#cond, __FILE__, __LINE__, __func__)
#endif

void (__attribute__((noreturn)) abort)(void);
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

extern inline _Bool (__attribute__((always_inline,gnu_inline,pure,__const__))
	IS_SANE_NONNULL)(__shadowed_value_t v)
{
	/* HACK based on where our shadow space assumes stuff is stored,
	 * with added exemption for small integers. */
	return (v && (
			   (v >= 0x000000400000ul && v <= 0x055500000000ul)
			|| (v >= 0x2aaa00000000ul && v <= 0x2fff00000000ul)
			|| (v >= 0x7aaa00000000ul && v <= 0x7ffffffffffful)
			));
}

extern inline void
(__attribute__((always_inline,gnu_inline,used,nonnull(1))) __store_shadow_nonlocal)
(void *dest, __shadowed_value_t v, __shadow_t shadow,
	void *arg)
{
	unsigned long dest_addr __attribute__((unused)) = (unsigned long) dest;
	unsigned long shadow_stored_addr __attribute__((unused))
	 = (unsigned long) __shadow_stored_addr(dest);
	if (IS_SANE_NONNULL(v) && !arg && !shadow.byte)
	{
		warnx("Storing non-null pointer with no provenance");
	}
	*(__raw_shadow_t *) shadow_stored_addr = (__raw_shadow_t) shadow.byte;
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,used,pure))
	__fetch_shadow_inl)(__shadowed_value_t v, const void *loaded_from, void *t)
{
	if (loaded_from)
	{
		__shadow_t s = (__shadow_t) { *(__raw_shadow_t *) __shadow_stored_addr(loaded_from) };
		if (IS_SANE_NONNULL(v) && !t && !s.byte)
		{
			warnx("Loaded non-null pointer with no provenance");
		}
		return s;
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
	assert(alignment_delta == 0);
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

/* More important messages get lower numbers.
 * We print a message if its number is <= the current trace level */

extern int __libcrunch_debug_level;
#define PRISHADOW "%02x"
#define PRINT_SHADOW_EXPR(s) (unsigned char)(long)*(char*)&s
#define TRACE_SHADOW(lvl, descr, val, args...) (((lvl)<=__libcrunch_debug_level) ? (void)warnx("(bsp=%p): shadow " PRISHADOW " " descr, __shadow_sp, PRINT_SHADOW_EXPR(val), ## args ) : (void)0)
#define TRACE_WORD(lvl, descr, w, args...) (((lvl)<=__libcrunch_debug_level) ? (void)warnx("(bsp=%p): word %lx " descr, __shadow_sp, w, ## args ) : (void)0 )
#define TRACE_STRING(lvl, descr, s, args...) (((lvl)<=__libcrunch_debug_level) ? (void)warnx("(bsp=%p): %s " descr, __shadow_sp, s, ## args ) : (void)0 )

extern inline __raw_shadow_t (__attribute__((always_inline,gnu_inline,pure,__const__))
		__shadow_from_word)(unsigned long arg)
{
	if (!arg) return NULL_SHADOW;
	__raw_shadow_t s = (char) (1 + 
			((arg >> 8) ^ (arg & 0xff))
				 % 254);
	return s;
}

extern inline __shadow_t (__attribute__((always_inline,gnu_inline,pure,__const__))
		__make_shadow)(__shadowed_value_t arg1, __shadowed_value_t arg2)
{
	/* The two arguments we get are (1) a pointer-sized value denoting some property
	 * of the allocation, and (2) an opaque word-sized value that refines this
	 * to denote the allocation uniquely. It might be zero.
	 * We take the second-from-bottom byte of the pointer and XOR it with the offset. */
	assert(arg2 == 0);
	if (!arg1) return (__shadow_t) { NULL_SHADOW };
	__raw_shadow_t made =__shadow_from_word(arg1);
	TRACE_SHADOW(1, "made fresh from pointer %p", made, (void*) arg1);
	/* CHECK against what liballocs thinks */
	if (__libcrunch_debug_level > 1)
	{
		void *base = __liballocs_get_alloc_base((void*) arg1);
		__raw_shadow_t expected = __shadow_from_word((unsigned long) base);
		assert(made == expected);
	}
	
	return (__shadow_t) { made };
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
	TRACE_STRING(1, "about to be called (cookie %p)", callee_name, callee);
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
		TRACE_SHADOW(1, "peeked for argument at offset %d", b, offset);
		return b;
	}
	TRACE_STRING(1, "no shadow for shadowed arg expr", debugstr);
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
		TRACE_SHADOW(1, "peeked as result at offset %d", b, offset);
		return b;
	}
	if (IS_SANE_NONNULL(v))
	{
		TRACE_STRING(0, " : callee returned no shadow (call site address: somewhere before %p)", calleestr, __get_pc());
	}
	return __make_invalid_shadow(v);
}

extern inline void (__attribute__((always_inline,gnu_inline,used)) __cleanup_shadow_stack)(void *saved_ptr);
extern inline void (__attribute__((always_inline,gnu_inline,used)) __cleanup_shadow_stack)(void *saved_ptr)
{
	__shadow_sp = (unsigned long *) saved_ptr;
	TRACE_STRING(1, "call sequence finished", "");
}

extern inline _Bool (__attribute__((always_inline,gnu_inline,used)) __check_deref)(const void *addr, __shadow_t derefed_shadow);
extern inline _Bool (__attribute__((always_inline,gnu_inline,used)) __check_deref)(const void *addr, __shadow_t derefed_shadow)
{
	if (!addr)
	{
		warnx("Null dereference");
		abort();
	}
	if (derefed_shadow.byte != 0 && derefed_shadow.byte != (char)255)
	{
		void *base = __liballocs_get_alloc_base((void*) addr);
		__raw_shadow_t base_shadow = __shadow_from_word((unsigned long) base);
		if (base_shadow == derefed_shadow.byte) return 1;
		warnx("Pointer with value %p has provenance %02x but points to non-matching object base %p (%02x)",
			addr, (unsigned)(unsigned char)derefed_shadow.byte, base, (unsigned)(unsigned char) base_shadow);
	}
	warnx("Bad provenance for %p: %x", addr, (unsigned)(unsigned char) derefed_shadow.byte);
	abort();
}
