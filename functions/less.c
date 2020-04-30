#include "ulisp.h"

object *fn_less (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(LESS, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(LESS, first(args));
    if (!(arg1 < arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}
