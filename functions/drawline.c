#include "ulisp.h"

object *fn_drawline (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWLINE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWLINE, car(args));
  tft.drawLine(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}
