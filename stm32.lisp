;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; STM32

(defparameter *header-stm32*
#"/* uLisp STM32 Version 2.9 - www.ulisp.com
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
#endif"#)

(defparameter *workspace-stm32* #"
// Workspace
#define PERSIST __attribute__((section(".text")))
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(_BOARD_MAPLE_MINI_H_)
  #define WORKSPACESIZE 1130-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 10240                /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes - must be even*/
  uint8_t _end;

#elif defined(_VARIANT_ARDUINO_STM32_)
  #define WORKSPACESIZE 1472-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 10240                /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes - must be even*/
  uint8_t _end;

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];"#)

(defparameter *stream-interface-stm32* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(_BOARD_MAPLE_MINI_H_) || defined(_VARIANT_ARDUINO_STM32_)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#endif
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return SDgfile.read();
}
#endif

void serialbegin (int address, int baud) {
  #if defined(_BOARD_MAPLE_MINI_H_) || defined(_VARIANT_ARDUINO_STM32_)
  if (address == 1) Serial1.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #endif
}

void serialend (int address) {
  #if defined(_BOARD_MAPLE_MINI_H_) || defined(_VARIANT_ARDUINO_STM32_)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #endif
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
  else if (streamtype == SPISTREAM) gfun = spiread;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    else if (address == 1) gfun = serial1read;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
inline void serial1write (char c) { Serial1.write(c); }
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) pfun = spiwrite;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    else if (address == 1) pfun = serial1write;
  }   
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}"#)


(defparameter *check-pins-stm32* #"
void checkanalogread (int pin) {
#if defined(_BOARD_MAPLE_MINI_H_)
  if (!(pin>=3 && pin<=11)) error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#elif defined(_VARIANT_ARDUINO_STM32_)
  if (!((pin>=0 && pin<=7) || pin==16)) error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(_BOARD_MAPLE_MINI_H_)
  if (!((pin>=3 && pin<=5) || (pin>=8 && pin<=11) || (pin>=15 && pin<=16) || (pin>=25 && pin<=27)))
    error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#elif defined(_VARIANT_ARDUINO_STM32_)
  if (!((pin>=0 && pin<=3) || (pin>=6 && pin<=10) || pin==16 || (pin>=22 && pin<=23)))
    error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#endif
}"#)


(defparameter *note-stm32* #"
// Note

void tone (int pin, int note) {
  (void) pin, (void) note;
}

void noTone (int pin) {
  (void) pin;
}

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
}

void nonote (int pin) {
  noTone(pin);
}"#)

(defparameter *sleep-stm32* #"
// Sleep

void initsleep () {

}

void sleep (int secs) {
  delay(1000*secs);
}"#)