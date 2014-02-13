#include <stdlib.h>

#include "stubgen.h"

// xcalloc(zZ)p
#define arglist_xcalloc(make_arg) make_arg(0, z), make_arg(1, Z)
make_wrapper(xcalloc, p)

// xmalloc(Z)p
#define arglist_xmalloc(make_arg) make_arg(0, Z)
make_wrapper(xmalloc, p)

// xrealloc(pZ)p
#define arglist_xrealloc(make_arg) make_arg(0, p), make_arg(1, Z)
make_wrapper(xrealloc, p)

// xmallocz(Z)
#define arglist_xmallocz(make_arg) make_arg(0, Z)
make_wrapper(xmallocz, p)
