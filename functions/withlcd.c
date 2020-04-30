#include "ulisp.h"

object *sp_withlcd (object *args, object *env) {
#if defined(__MSP430FR6989__)
  myLCD.init();
  object *params = first(args);
  object *var = first(params);
  object *pair = cons(var, stream(LCDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
#else
  (void) args, (void) env;
  error(PSTR("with-lcd not supported"));
  return nil;
#endif
}
