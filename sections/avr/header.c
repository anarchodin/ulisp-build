/* uLisp AVR Version 2.9 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 16th September 2019

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "";

// Compile options

#define checkoverflow
// #define resetautorun
#define printfreespace
#define serialmonitor
// #define printgcs
// #define sdcardsupport
// #define lisplibrary
// #define lineeditor
// #define vt100

// Includes

// #include "LispLibrary.h"
#include <avr/sleep.h>
#include <setjmp.h>
#include <SPI.h>
#include <limits.h>
#include <EEPROM.h>

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif

#if __AVR_ARCH__ == 100 || __AVR_ARCH__ == 103
#define PROGMEM
#define PSTR(s) (s)
#else
#define NEEDS_PROGMEM
#endif