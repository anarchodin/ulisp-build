#include "ulisp.h"

#if defined(gfxsupport)
void Technoblogy_ST7735::xorPixel (uint16_t x, uint16_t y, uint16_t color) {
  uint16_t lastcolor = getPixel(x, y);
  if ((x >= 0) && (x < _width) && (y >= 0) && (y < _height)) {
    startWrite();
    writeCommand(ST77XX_RAMWR);
    SPI_WRITE16(color ^ lastcolor);
    endWrite();
  }
}
#endif

object *fn_xorpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(XORPIXEL, third(args));
  tft.xorPixel(checkinteger(XORPIXEL, first(args)), checkinteger(XORPIXEL, second(args)), colour);
  return nil;
#endif
}
