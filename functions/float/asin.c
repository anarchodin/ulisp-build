#include "ulisp.h"

object *fn_asin (object *args, object *env) {
  (void) env;
  return makefloat(asin(checkintfloat(ASIN, first(args))));
}
