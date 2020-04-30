#include "ulisp.h"

object *fn_abs (object *args, object *env) {
  (void) env;
  int result = checkinteger(ABS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(ABS, overflow);
  #endif
  return number(abs(result));
}
