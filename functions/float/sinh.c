#include "ulisp.h"

object *fn_sinh (object *args, object *env) {
  (void) env;
  return makefloat(sinh(checkintfloat(SINH, first(args))));
}
