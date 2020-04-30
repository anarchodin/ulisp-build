#include "ulisp.h"

object *fn_arraydimensions (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(ARRAYDIMENSIONS, PSTR("argument is not an array"), array);
  return cddr(array);
}
