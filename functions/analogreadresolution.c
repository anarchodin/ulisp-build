#include "ulisp.h"

object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogReadResolution(checkinteger(ANALOGREADRESOLUTION, arg));
  return arg;
}
