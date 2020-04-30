#include "ulisp.h"

object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(PINMODE, first(args));
  int pm = INPUT;
  object *mode = second(args);
  if (integerp(mode)) {
    int nmode = checkinteger(PINMODE, mode);
    if (nmode == 1) pm = OUTPUT; else if (nmode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (nmode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (mode != nil) pm = OUTPUT;
  pinMode(pin, (WiringPinMode)pm);
  return nil;
}
