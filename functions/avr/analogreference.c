#include "ulisp.h"

object *fn_analogreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  analogReference(checkkeyword(ANALOGREFERENCE, arg));
  return arg;
}
