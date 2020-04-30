#include "ulisp.h"

object *fn_subtract (object *args, object *env) {
  (void) env;
  int result = checkinteger(SUBTRACT, car(args));
  args = cdr(args);
  if (args == NULL) {
    #if defined(checkoverflow)
    if (result == INT_MIN) error2(SUBTRACT, overflow);
    #endif
    return number(-result);
  }
  while (args != NULL) {
    int temp = checkinteger(SUBTRACT, car(args));
    #if defined(checkoverflow)
    if (temp < 1) { if (INT_MAX + temp < result) error2(SUBTRACT, overflow); }
    else { if (INT_MIN + temp > result) error2(SUBTRACT, overflow); }
    #endif
    result = result - temp;
    args = cdr(args);
  }
  return number(result);
}
