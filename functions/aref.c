#include "ulisp.h"

object *fn_aref (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
  object *dimensions = cddr(array);
  int y = 0, x = checkinteger(AREF, second(args));
  if (listp(dimensions)) {
    if (cddr(args) == NULL) error2(AREF, PSTR("array needs two subscripts"));
    y = checkinteger(AREF, third(args));
  } else if (cddr(args) != NULL) error2(AREF, PSTR("array needs one subscript"));
  return *getarray(AREF, array, x, y);
}
