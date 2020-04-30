#include "ulisp.h"

object *fn_drawcircle (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(DRAWCIRCLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWCIRCLE, car(args));
  tft.drawCircle(params[0], params[1], params[2], colour);
  return nil;
#endif
}
