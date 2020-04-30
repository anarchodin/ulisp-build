#include "ulisp.h"

object *fn_shiftout (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  object *value = fourth(args);
  shiftOut(datapin, clockpin, order, integer(value));
  return value;
}
