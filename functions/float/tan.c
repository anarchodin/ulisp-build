#include "ulisp.h"

object *fn_tan (object *args, object *env) {
  (void) env;
  return makefloat(tan(checkintfloat(TAN, first(args))));
}
