#include "ulisp.h"

object *fn_arraydimensions (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(ARRAYDIMENSIONS, PSTR("argument is not an array"), array);
  object *dimensions = cddr(array);
  return (first(dimensions)->integer < 0) ? cons(number(-(first(dimensions)->integer)), cdr(dimensions)) : dimensions;
}
