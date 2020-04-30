#include "ulisp.h"

object *fn_cadr (object *args, object *env) {
  (void) env;
  return carx(cdrx(first(args)));
}
