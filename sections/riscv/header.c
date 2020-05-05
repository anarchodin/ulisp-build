/* uLisp RISC-V 3.2 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 23rd April 2020

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

// #define resetautorun
#define printfreespace
#define serialmonitor
// #define printgcs
#define sdcardsupport
#define gfxsupport
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
#include <Sipeed_ST7789.h>
SPIClass spi_(SPI0); // MUST be SPI0 for Maix series on board LCD
Sipeed_ST7789 tft(320, 240, spi_);
#endif

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif
