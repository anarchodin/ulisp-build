#include "ulisp.h"

object *fn_greatereq (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(GREATEREQ, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(GREATEREQ, first(args));
    if (!(arg1 >= arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}
