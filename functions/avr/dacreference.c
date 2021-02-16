#include "ulisp.h"

object *fn_dacreference (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  #if defined(CPU_AVR128DX48)
  int ref = checkinteger(DACREFERENCE, arg);
  DACReference(ref);
  #endif
  return arg;
}
