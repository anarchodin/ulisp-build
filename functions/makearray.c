#include "ulisp.h"

object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  int xd, yd = 1;
  object *dimensions = first(args);
  if (dimensions == NULL) error2(MAKEARRAY, PSTR("dimensions can't be nil"));
  else if (atom(dimensions)) dimensions = cons(dimensions, NULL);
 
  if (cdr(dimensions) == NULL) xd = checkinteger(MAKEARRAY, first(dimensions));
  else if (cddr(dimensions) == NULL) {
    xd = checkinteger(MAKEARRAY, first(dimensions));
    yd = checkinteger(MAKEARRAY, second(dimensions));
  } else error2(MAKEARRAY, PSTR("only two dimensions supported"));
  if (xd < 0 || yd < 0) error2(MAKEARRAY, PSTR("dimension can't be negative"));
  if (cdr(args) != NULL) {
    object *var = second(args);
    if (!symbolp(var) || var->name != INITIALELEMENT)
      error(MAKEARRAY, PSTR("illegal second argument"), var); 
    if (cddr(args) != NULL) def = third(args);
  }
  return makearray(xd, yd, dimensions, def);
}
