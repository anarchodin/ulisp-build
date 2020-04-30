#include "ulisp.h"

object *sp_push (object *args, object *env) {
  checkargs(PUSH, args); 
  object *item = eval(first(args), env);
  object **loc = place(PUSH, second(args), env);
  push(item, *loc);
  return *loc;
}
