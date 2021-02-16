#include "ulisp.h"

object *fn_analogreadresolution (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(CPU_AVR128DX48)
  uint8_t res = checkinteger(ANALOGREADRESOLUTION, arg);
  if (res == 10) analogReadResolution(10);
  else if (res == 12) analogReadResolution(12);
  else error(ANALOGREADRESOLUTION, PSTR("invalid resolution"), res);
  #else
  error2(ANALOGREADRESOLUTION, PSTR("not supported"));
  #endif
  return arg;
}
