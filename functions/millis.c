#include "ulisp.h"

object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}
