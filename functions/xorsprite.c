#include "ulisp.h"

#if defined(gfxsupport)
void Technoblogy_ST7735::xorSprite (uint16_t x, uint16_t y, uint32_t top, uint32_t bottom, uint16_t color) {
  uint16_t row[8];
  uint32_t col = 0;
  bool bit;
  if ((x >= 0) && (x+7 < _width) && (y >= 0) && (y+7 < _height)) {
    for (int yd=0; yd<8; yd++) {
      startWrite();
      setAddrWindow(x, y+yd, 8, 1);
      writeCommand(ST77XX_RAMRD);
      pinMode(TFT_MOSI, INPUT);
      pinMode(TFT_SCLK, OUTPUT);
      for (int i=0; i<9; i++) {
        digitalWrite(TFT_SCLK, HIGH);
        digitalWrite(TFT_SCLK, LOW);
      }
      for (int xd=0; xd<8; xd++) {
        for (int i=0; i<24; i++) {
          digitalWrite(TFT_SCLK, HIGH);
          col = col<<1 | digitalRead(TFT_MOSI);
          digitalWrite(TFT_SCLK, LOW);
        }
        row[xd] = ((col & 0xf80000)>>8 | (col & 0xfc00)>>5 | (col & 0xf8)>>3);
      }
      pinMode(TFT_MOSI, OUTPUT);
      endWrite();
      startWrite();
      writeCommand(ST77XX_RAMWR);
      for (int xd=0; xd<8; xd++) {
        if (yd < 4) bit = top>>(31 - xd - yd*8) & 1;
        else bit = bottom>>(31 - xd - (yd-4)*8) & 1;
        if (bit) SPI_WRITE16(row[xd] ^ color);
        else SPI_WRITE16(row[xd]);
      }
      endWrite();
    }
  }
}
#endif

object *fn_xorsprite (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint32_t params[4]; uint16_t colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(XORSPRITE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(XORSPRITE, car(args));
  tft.xorSprite(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}
