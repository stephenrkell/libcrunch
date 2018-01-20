#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/memory.h>
#include <caml/callback.h>

// HACK: internal OCaml Unix module detail
value unix_error_of_code(int code);

CAMLprim value
caml_mkstemp(value templ)
{
	CAMLparam1(templ);
	const char *s = String_val(templ);
	char *s_dup = strdup(s);
	int fd = mkstemp(s_dup);
	if (fd == -1)
	{
		/* raise the Unix.Unix_error for our errno -- 
		   Leroy and Remi say
		   (https://ocaml.github.io/ocamlunix/ocamlunix.pdf)
		
			"Unless otherwise indicated, all functions in the Unix module raise the exception
			Unix_error in case of error.
			
			exception Unix_error of error * string * string
			
			The second argument of the Unix_error exception is the name of the system call
			that raised the error. The third argument identifies, if possible, the object on
			which the error occurred; for example, in the case of a system call taking a file
			name as an argument, this file name will be in the third position in Unix_error.
			Finally, the first argument of the exception is an error code indicating the nature
			of the error. It belongs to the variant type error:
			
			type error = E2BIG | EACCES | EAGAIN | ... | EUNKNOWNERR of int
		 */
		CAMLlocal1(tup);
		tup = caml_alloc(3, 0);
		Store_field(tup, 0, unix_error_of_code(errno));
		Store_field(tup, 1, caml_copy_string("mkstemp"));
		Store_field(tup, 2, templ);
		value exn = *caml_named_value("Unix.Unix_error");

		free(s_dup);
		caml_raise_with_arg(exn, Val_int(errno));
	}
	
	CAMLlocal1(out);
	out = caml_alloc(2, 0);
	Store_field(out, 0, fd);
	Store_field(out, 1, caml_copy_string(s_dup));
	free(s_dup);
	CAMLreturn(out);
	//return out; /* HACK: assuming it's a mere int */
}
