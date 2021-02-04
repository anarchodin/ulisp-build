#include "ulisp.h"

object *fn_break (object *args, object *env) {
  (void) args;
  pfstring(PSTR("\nBreak!\n"), pserial);
  BreakLevel++;
  repl(env);
  BreakLevel--;
  return nil;
}
