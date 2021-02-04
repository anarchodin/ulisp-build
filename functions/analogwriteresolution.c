#include "ulisp.h"

object *fn_analogwriteresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogWriteResolution(checkinteger(ANALOGWRITERESOLUTION, arg));
  return arg;
}
