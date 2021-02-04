#include "ulisp.h"

object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALWRITE, first(args));
  object *arg = second(args);
  int mode;
  if (keywordp(arg)) mode = checkkeyword(DIGITALWRITE, arg);
  else if (integerp(arg)) mode = arg->integer ? HIGH : LOW;
  else mode = (arg != nil) ? HIGH : LOW;
  digitalWrite(pin, mode);
  return arg;
}
