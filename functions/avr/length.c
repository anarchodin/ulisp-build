#include "ulisp.h"

object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (!stringp(arg)) error(LENGTH, PSTR("argument is not a list or string"), arg);
  return number(stringlength(arg));
}
