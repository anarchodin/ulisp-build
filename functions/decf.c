#include "ulisp.h"

/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls incfdecf().
*/
object *sp_decf (object *args, object *env) {
  incfdecf(DECF, args, -1, env);
}
