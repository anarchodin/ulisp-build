#include "ulisp.h"

object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, 0x11, args);
  return first(args);
}
