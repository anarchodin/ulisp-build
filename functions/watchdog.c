#include "ulisp.h"

object *fn_watchdog (object *args, object *env) {
  (void) env;
  if (args == NULL) watchdogreset();
  else watchdogenable(integer(first(args)));
  return nil;
}
