#include "ulisp.h"

object *sp_trace (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *var = first(args);
    if (!symbolp(var)) error(TRACE, notasymbol, var);
    trace(var->name);
    args = cdr(args);
  }
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
    i++;
  }
  return args;
}
