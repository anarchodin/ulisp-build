#include "ulisp.h"

object *fn_fillscreen (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(FILLSCREEN, first(args));
  tft.fillScreen(colour);
  return nil;
#endif
}
