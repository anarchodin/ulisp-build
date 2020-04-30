#include "ulisp.h"

object *fn_xorsprite (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(XORSPRITE, PSTR("not supported"));
  return nil;
#endif
}
