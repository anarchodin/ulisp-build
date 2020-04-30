#include "ulisp.h"

object *fn_minfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(MINFN, first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(MINFN, car(args));
    if (next < result) result = next;
    args = cdr(args);
  }
  return number(result);
}
