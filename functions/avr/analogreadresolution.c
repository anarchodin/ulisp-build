#include "ulisp.h"

object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(CPU_AVR128DA48)
  analogReadResolution(checkinteger(ANALOGREADRESOLUTION, arg));
  #else
  error2(ANALOGREADRESOLUTION, PSTR("not supported"));
  #endif
  return arg;
}
