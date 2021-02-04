/* uLisp ESP Version 3.4 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 4th January 2021

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

// #define resetautorun
#define printfreespace
// #define printgcs
// #define sdcardsupport
// #define gfxsupport
// #define lisplibrary
// #define lineeditor
// #define vt100

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <EEPROM.h>
#if defined (ESP8266)
  #include <ESP8266WiFi.h>
#elif defined (ESP32)
  #include <WiFi.h>
#endif

#if defined(gfxsupport)
#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_SSD1306.h>
#define COLOR_WHITE 1
#define COLOR_BLACK 0
#define SCREEN_WIDTH 128 // OLED display width, in pixels
#define SCREEN_HEIGHT 64 // OLED display height, in pixels
#define OLED_RESET     4
Adafruit_SSD1306 tft(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire);
#endif

#if defined(sdcardsupport)
  #include <SD.h>
  #define SDSIZE 172
#else
  #define SDSIZE 0
#endif
