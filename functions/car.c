#include "ulisp.h"

object *fn_car (object *args, object *env) {
  (void) env;
  return carx(first(args));
}
