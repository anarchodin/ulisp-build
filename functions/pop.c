#include "ulisp.h"

object *sp_pop (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  checkargs(POP, 0x11, args);
  #ifdef ARRAY
  object **loc = place(POP, first(args), env, &bit);
  #else
  object **loc = place(POP, first(args), env);
  #endif
  object *result = car(*loc);
  pop(*loc);
  return result;
}
