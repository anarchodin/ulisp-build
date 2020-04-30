#include "ulisp.h"

object *fn_drawrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWRECT, car(args));
  tft.drawRect(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}
