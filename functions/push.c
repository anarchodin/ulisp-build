#include "ulisp.h"

object *sp_push (object *args, object *env) {
  int bit;
  checkargs(PUSH, args); 
  object *item = eval(first(args), env);
  object **loc = place(PUSH, second(args), env, &bit);
  push(item, *loc);
  return *loc;
}
