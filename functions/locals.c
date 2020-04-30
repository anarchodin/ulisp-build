#include "ulisp.h"

object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}
