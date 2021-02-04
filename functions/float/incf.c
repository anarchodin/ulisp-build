#include "ulisp.h"

/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_incf (object *args, object *env) {
  int bit;
  checkargs(INCF, args); 
  object **loc = place(INCF, first(args), env, &bit);
  args = cdr(args);
  
  object *x = *loc;
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int increment;
    if (inc == NULL) increment = 1; else increment = checkbitvalue(INCF, inc);
    int newvalue = (((*loc)->integer)>>bit & 1) + increment;

    if (newvalue & ~1) error2(INCF, PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }

  if (floatp(x) || floatp(inc)) {
    float increment;
    float value = checkintfloat(INCF, x);

    if (inc == NULL) increment = 1.0; else increment = checkintfloat(INCF, inc);

    *loc = makefloat(value + increment);
  } else if (integerp(x) && (integerp(inc) || inc == NULL)) {
    int increment;
    int value = x->integer;

    if (inc == NULL) increment = 1; else increment = inc->integer;

    if (increment < 1) {
      if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    } else {
      if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    }
  } else error2(INCF, notanumber);
  return *loc;
}
