#include "ulisp.h"

object *fn_drawpixel (object *args, object *env) {
  #if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(DRAWPIXEL, third(args));
  tft.drawPixel(checkinteger(DRAWPIXEL, first(args)), checkinteger(DRAWPIXEL, second(args)), colour);
  #endif
  return nil;
}
