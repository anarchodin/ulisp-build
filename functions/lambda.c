#include "ulisp.h"

object *sp_lambda (object *args, object *env) {
  return cons(symbol(CLOSURE), (cons(env,args)));
}
