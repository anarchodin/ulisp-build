#include "ulisp.h"

object *fn_sin (object *args, object *env) {
  (void) env;
  return makefloat(sin(checkintfloat(SIN, first(args))));
}
