#include "ulisp.h"

object *fn_room (object *args, object *env) {
  (void) args, (void) env;
  return number(Freespace);
}
