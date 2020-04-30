#include "ulisp.h"

object *fn_fillrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(FILLRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLRECT, car(args));
  tft.fillRect(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}
