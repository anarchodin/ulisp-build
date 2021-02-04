#include "ulisp.h"

object *sp_setf (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, oddargs);
    #ifdef ARRAY
    object **loc = place(SETF, first(args), env, &bit);
    #else
    object **loc = place(SETF, first(args), env);
    #endif
    arg = eval(second(args), env);
    #ifdef ARRAY
    if (bit == -1) *loc = arg;
    else *loc = number((checkinteger(SETF,*loc) & ~(1<<bit)) | checkbitvalue(SETF,arg)<<bit);
    #else
    *loc = arg;
    #endif
    args = cddr(args);
  }
  return arg;
}
