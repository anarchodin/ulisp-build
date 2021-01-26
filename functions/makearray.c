#include "ulisp.h"

object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  object *dims = first(args);
  if (dims == NULL) error2(MAKEARRAY, PSTR("dimensions can't be nil"));
  else if (atom(dims)) dims = cons(dims, NULL);
  if (cdr(args) != NULL) {
    object *var = second(args);
    if (!symbolp(var) || var->name != INITIALELEMENT)
      error(MAKEARRAY, PSTR("illegal second argument"), var); 
    if (cddr(args) != NULL) def = third(args);
  }
  return makearray(MAKEARRAY, dims, def);
}
