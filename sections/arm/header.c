/* uLisp ARM 3.2 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - unreleased

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

// #define resetautorun
#define printfreespace
#define serialmonitor
// #define printgcs
// #define sdcardsupport
// #define gfxsupport
// #define lisplibrary
#define assemblerlist
// #define lineeditor
// #define vt100

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>

#if defined(gfxsupport)
#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_ST7735.h> // Hardware-specific library for ST7735
#define COLOR_WHITE 0xffff
#define COLOR_BLACK 0

// Adafruit PyBadge/PyGamer
#define TFT_CS        44  // Chip select
#define TFT_RST       46  // Display reset
#define TFT_DC        45  // Display data/command select
#define TFT_BACKLIGHT 47  // Display backlight pin
#define TFT_MOSI      41  // Data out
#define TFT_SCLK      42  // Clock out

class Technoblogy_ST7735 : public Adafruit_ST7735 {       
public:
  Technoblogy_ST7735(int8_t cs, int8_t dc, int8_t mosi, int8_t sclk, int8_t rst);
  uint16_t getPixel(uint16_t x, uint16_t y);
  void xorPixel(uint16_t x, uint16_t y, uint16_t color);
};

Technoblogy_ST7735::Technoblogy_ST7735(int8_t cs, int8_t dc, int8_t mosi, int8_t sclk, int8_t rst)
    : Adafruit_ST7735(cs, dc, mosi, sclk, rst) {}

Technoblogy_ST7735 tft = Technoblogy_ST7735(TFT_CS, TFT_DC, TFT_MOSI, TFT_SCLK, TFT_RST);
#endif

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif
