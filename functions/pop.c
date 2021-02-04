#include "ulisp.h"

object *sp_pop (object *args, object *env) {
  int bit;
  checkargs(POP, args); 
  object **loc = place(POP, first(args), env, &bit);
  object *result = car(*loc);
  pop(*loc);
  return result;
}
