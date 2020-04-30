#include "ulisp.h"

object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGREAD, first(args));
  checkanalogread(pin);
  return number(analogRead(pin));
}
