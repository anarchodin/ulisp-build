#include "ulisp.h"

object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}
