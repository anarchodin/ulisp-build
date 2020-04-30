#include "ulisp.h"

object *fn_settextwrap (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setTextWrap(first(args) != NULL);
  return nil;
#endif
}
