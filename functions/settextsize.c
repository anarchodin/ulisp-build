#include "ulisp.h"

object *fn_settextsize (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setTextSize(checkinteger(SETTEXTSIZE, first(args)));
  return nil;
#endif
}
