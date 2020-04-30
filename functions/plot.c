#include "ulisp.h"

void plotsub (uint8_t x, uint8_t y, uint8_t n, int ys[5]) {
  if (y>=0 && y<64) {
    uint8_t grey = 0x0F-n*3;
    uint8_t blob = grey;
    if ((x&1) == 0) { blob = grey<<4; ys[n] = y; }
    else {
      for (int i=0; i<5; i++) {
        if (y == ys[i]) blob = (0x0F-i*3)<<4 | grey; 
      }
    }
    PlotByte(x>>1, y, blob);
  }
}

object *fn_plot (object *args, object *env) {
  int ys[5] = {-1, -1, -1, -1, -1};
  int xaxis = -1, yaxis = -1;
  delay(20);
  ClearDisplay(); // Clear display
  if (args != NULL && integerp(first(args))) { xaxis = checkinteger(PLOT, first(args)); args = cdr(args); }
  if (args != NULL && integerp(first(args))) { yaxis = checkinteger(PLOT, first(args)); args = cdr(args); }
  int nargs = min(listlength(PLOT, args),4);
  for (int x=0; x<256; x++) {
    object *rest = args;
    for (int n=0; n<nargs; n++) {
      object *function = first(rest);
      int y = checkinteger(PLOT, apply(PLOT, function, cons(number(x), NULL), env));
      plotsub(x, y, n+1, ys);
      rest = cdr(rest);
    }
    plotsub(x, yaxis, 0, ys);
    if (x == xaxis) for (int y=0; y<64; y++) plotsub(x, y, 0, ys);
    if ((x&1) != 0) for (int i=0; i<5; i++) ys[i] = -1;
  }
  while (!tstflag(ESCAPE)); clrflag(ESCAPE);
  return symbol(NOTHING);
}
