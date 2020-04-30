#include "ulisp.h"

object *fn_shiftin (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  int value = shiftIn(datapin, clockpin, order);
  return number(value);
}
