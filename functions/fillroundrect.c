#include "ulisp.h"

object *fn_fillroundrect (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(FILLROUNDRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLROUNDRECT, car(args));
  tft.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  #endif
  return nil;
}
