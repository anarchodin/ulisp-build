#include "ulisp.h"

object *fn_aref (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
  return *getarray(AREF, array, cdr(args), 0);
}
