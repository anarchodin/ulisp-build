#include "ulisp.h"

object *fn_numeq (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(NUMEQ, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(NUMEQ, first(args));
    if (!(arg1 == arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}
