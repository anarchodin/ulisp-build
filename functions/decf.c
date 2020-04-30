#include "ulisp.h"

/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_decf (object *args, object *env) {
  checkargs(DECF, args); 
  object **loc = place(DECF, first(args), env);
  int decrement = 1;
  int result = checkinteger(DECF, *loc);
  args = cdr(args);
  if (args != NULL) decrement = checkinteger(DECF, eval(first(args), env));
  #if defined(checkoverflow)
  if (decrement < 1) { if (INT_MAX + decrement < result) error2(DECF, overflow); }
  else { if (INT_MIN + decrement > result) error2(DECF, overflow); }
  #endif
  result = result - decrement;
  *loc = number(result);
  return *loc;
}
