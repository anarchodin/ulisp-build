#include "ulisp.h"

object *fn_cdar (object *args, object *env) {
  (void) env;
  return cdrx(carx(first(args)));
}
