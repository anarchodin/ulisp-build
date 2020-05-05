/* uLisp MSP430 Version 2.7 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 24th May 2019

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

// Includes

// #include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>

#if defined(sdcardsupport)
#include <SD.h>
#endif
#define strcasecmp_P(a, b) strcasecmp((a), (b))
#define yield()
typedef int BitOrder;
