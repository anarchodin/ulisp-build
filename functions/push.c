#include "ulisp.h"

object *sp_push (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  checkargs(PUSH, args); 
  object *item = eval(first(args), env);
  #ifdef ARRAY
  object **loc = place(PUSH, second(args), env, &bit);
  #else
  object **loc = place(PUSH, second(args), env);
  #endif
  push(item, *loc);
  return *loc;
}
