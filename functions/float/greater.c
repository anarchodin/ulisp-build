#include "ulisp.h"

object *fn_greater (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) > (arg2->integer))) return nil;
    } else if (!(checkintfloat(GREATER, arg1) > checkintfloat(GREATER, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}