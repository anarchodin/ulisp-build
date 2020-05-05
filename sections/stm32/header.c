/* uLisp STM32 Version 2.9 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 25th September 2019

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
// #define lisplibrary

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <EEPROM.h>

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif
