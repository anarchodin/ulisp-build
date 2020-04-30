#include "ulisp.h"

object *fn_caar (object *args, object *env) {
  (void) env;
  return carx(carx(first(args)));
}
