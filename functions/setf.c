#include "ulisp.h"

object *sp_setf (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, oddargs);
    object **loc = place(SETF, first(args), env);
    arg = eval(second(args), env);
    *loc = arg;
    args = cddr(args);
  }
  return arg;
}
