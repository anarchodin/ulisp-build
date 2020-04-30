#include "ulisp.h"

object *fn_setrotation (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setRotation(checkinteger(SETROTATION, first(args)));
  return nil;
#endif
}
