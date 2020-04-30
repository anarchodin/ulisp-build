#include "ulisp.h"

object *fn_xorpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(XORPIXEL, PSTR("not supported"));
  return nil;
#endif
}
