#include "ulisp.h"

object *fn_divide (object *args, object *env) {
  (void) env;
  int result = checkinteger(DIVIDE, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg = checkinteger(DIVIDE, car(args));
    if (arg == 0) error2(DIVIDE, PSTR("division by zero"));
    #if defined(checkoverflow)
    if ((result == INT_MIN) && (arg == -1)) error2(DIVIDE, overflow);
    #endif
    result = result / arg;
    args = cdr(args);
  }
  return number(result);
}
