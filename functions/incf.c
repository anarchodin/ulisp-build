#include "ulisp.h"

/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_incf (object *args, object *env) {
  checkargs(INCF, args); 
  object **loc = place(INCF, first(args), env);
  int increment = 1;
  int result = checkinteger(INCF, *loc);
  args = cdr(args);
  if (args != NULL) increment = checkinteger(INCF, eval(first(args), env));
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(INCF, overflow); }
  else { if (INT_MAX - increment < result) error2(INCF, overflow); }
  #endif
  result = result + increment;
  *loc = number(result);
  return *loc;
}
