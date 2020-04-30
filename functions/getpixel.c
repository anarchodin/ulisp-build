#include "ulisp.h"

#if defined(gfxsupport)
uint16_t Technoblogy_ST7735::getPixel (uint16_t x, uint16_t y) {
  uint32_t ret = 0;
  startWrite();
  setAddrWindow(x, y, 1, 1);
  writeCommand(ST77XX_RAMRD);
  pinMode(TFT_MOSI, INPUT);
  pinMode(TFT_SCLK, OUTPUT);
  for (int i=0; i<33; i++) {
    digitalWrite(TFT_SCLK, HIGH);
    ret = ret<<1 | digitalRead(TFT_MOSI);
    digitalWrite(TFT_SCLK, LOW);
  }
  pinMode(TFT_MOSI, OUTPUT);
  endWrite();
  return ((ret & 0xf80000)>>8 | (ret & 0xfc00)>>5 | (ret & 0xf8)>>3);
}
#endif

object *fn_getpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  return number(tft.getPixel(checkinteger(DRAWPIXEL, first(args)), checkinteger(DRAWPIXEL, second(args))));
#endif
}
