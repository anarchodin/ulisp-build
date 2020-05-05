/* uLisp ESP Version 3.1 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 28th February 2020

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] = "";

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
#if defined (ESP8266)
  #include <ESP8266WiFi.h>
#elif defined (ESP32)
  #include <WiFi.h>
#endif

#if defined(sdcardsupport)
  #include <SD.h>
  #define SDSIZE 172
#else
  #define SDSIZE 0
#endif
