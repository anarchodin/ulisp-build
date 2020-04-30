#include "ulisp.h"

object *fn_numberp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}
