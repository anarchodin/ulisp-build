#include "ulisp.h"

object *fn_greater (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(GREATER, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(GREATER, first(args));
    if (!(arg1 > arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}
