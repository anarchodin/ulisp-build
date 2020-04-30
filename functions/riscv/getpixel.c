#include "ulisp.h"

object *fn_getpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(GETPIXEL, PSTR("not supported"));
  return nil;
#endif
}
