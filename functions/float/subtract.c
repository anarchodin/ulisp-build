#include "ulisp.h"

object *subtract_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult - checkintfloat(SUBTRACT, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *negate (object *arg) {
  if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(-result);
    else return number(-result);
  } else if (floatp(arg)) return makefloat(-(arg->single_float));
  else error(SUBTRACT, notanumber, arg);
  return nil;
}

object *fn_subtract (object *args, object *env) {
  (void) env;
  object *arg = car(args);
  args = cdr(args);
  if (args == NULL) return negate(arg);
  else if (floatp(arg)) return subtract_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) return subtract_floats(args, result);
      else if (integerp(arg)) {
        int val = (car(args))->integer;
        if (val < 1) { if (INT_MAX + val < result) return subtract_floats(args, result); }
        else { if (INT_MIN + val > result) return subtract_floats(args, result); }
        result = result - val;
      } else error(SUBTRACT, notanumber, arg);
      args = cdr(args);
    }
    return number(result);
  } else error(SUBTRACT, notanumber, arg);
  return nil;
}
