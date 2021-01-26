#include "ulisp.h"

object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (stringp(arg)) return number(stringlength(arg));
  if (arrayp(arg) && cdr(cddr(arg)) == NULL) return first(cddr(arg));
  error(LENGTH, PSTR("argument is not a list, 1d array, or string"), arg);
}
