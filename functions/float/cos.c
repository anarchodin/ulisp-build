#include "ulisp.h"

object *fn_cos (object *args, object *env) {
  (void) env;
  return makefloat(cos(checkintfloat(COS, first(args))));
}
