#include "ulisp.h"

object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALWRITE, first(args));
  object *mode = second(args);
  if (integerp(mode)) digitalWrite(pin, mode->integer ? HIGH : LOW);
  else digitalWrite(pin, (mode != nil) ? HIGH : LOW);
  return mode;
}
