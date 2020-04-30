#include "ulisp.h"

object *fn_cdr (object *args, object *env) {
  (void) env;
  return cdrx(first(args));
}
