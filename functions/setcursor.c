#include "ulisp.h"

object *fn_setcursor (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setCursor(checkinteger(SETCURSOR, first(args)), checkinteger(SETCURSOR, second(args)));
  return nil;
#endif
}
