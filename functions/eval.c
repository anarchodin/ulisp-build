#include "ulisp.h"

object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}
