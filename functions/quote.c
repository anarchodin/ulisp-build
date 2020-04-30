#include "ulisp.h"

object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, args); 
  return first(args);
}
