#include "ulisp.h"

object *fn_sqrt (object *args, object *env) {
  (void) env;
  return makefloat(sqrt(checkintfloat(SQRT, first(args))));
}
