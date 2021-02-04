#include "ulisp.h"

object *fn_analogreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620)
  error2(ANALOGREFERENCE, PSTR("not supported"));
  #else
  analogReference((eAnalogReference)checkkeyword(ANALOGREFERENCE, arg));
  #endif
  return arg;
}
