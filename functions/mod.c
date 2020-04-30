#include "ulisp.h"

object *fn_mod (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(MOD, first(args));
  int arg2 = checkinteger(MOD, second(args));
  if (arg2 == 0) error2(MOD, PSTR("division by zero"));
  int r = arg1 % arg2;
  if ((arg1<0) != (arg2<0)) r = r + arg2;
  return number(r);
}
