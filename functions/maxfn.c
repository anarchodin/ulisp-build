#include "ulisp.h"

object *fn_maxfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(MAXFN, first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(MAXFN, car(args));
    if (next > result) result = next;
    args = cdr(args);
  }
  return number(result);
}
