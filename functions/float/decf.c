#include "ulisp.h"

/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_decf (object *args, object *env) {
  int bit;
  checkargs(DECF, args);
  object **loc = place(DECF, first(args), env, &bit);
  args = cdr(args);
  
  object *x = *loc;
  object *dec = (args != NULL) ? eval(first(args), env) : NULL;

  if (bit != -1) {
    int decrement;
    if (dec == NULL) decrement = 1; else decrement = checkbitvalue(DECF, dec);
    int newvalue = (((*loc)->integer)>>bit & 1) - decrement;

    if (newvalue & ~1) error2(INCF, PSTR("result is not a bit value"));
    *loc = number((((*loc)->integer) & ~(1<<bit)) | newvalue<<bit);
    return number(newvalue);
  }
  
  if (floatp(x) || floatp(dec)) {
    float decrement;
    float value = checkintfloat(DECF, x);

    if (dec == NULL) decrement = 1.0; else decrement = checkintfloat(DECF, dec);

    *loc = makefloat(value - decrement);
  } if (integerp(x) && (integerp(dec) || dec == NULL)) {
    int decrement;
    int value = x->integer;

    if (dec == NULL) decrement = 1; else decrement = dec->integer;

    if (decrement < 1) {
      if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    } else {
      if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    }
  } else error2(DECF, notanumber);
  return *loc;
}
