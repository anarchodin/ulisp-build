#include "ulisp.h"

object *tf_or (object *args, object *env) {
  while (args != NULL) {
    if (eval(car(args), env) != NULL) return car(args);
    args = cdr(args);
  }
  return nil;
}
