;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

; MSP430

(defparameter *header-msp430*
#"/* uLisp MSP430 Version 2.7 - www.ulisp.com
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
typedef int BitOrder;"#)

(defparameter *constants-msp430* #"
// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZERO=0, SYMBOL=2, NUMBER=4, STREAM=6, CHARACTER=8, FLOAT=10, STRING=12, PAIR=14 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, LCDSTREAM };"#)

(defparameter *workspace-msp430* #"
// Workspace
#define PERSIST __attribute__((section(".text")))
#define WORDALIGNED __attribute__((aligned (2)))
#define BUFFERSIZE 18

#if defined(__MSP430F5529__)
  #define WORKSPACESIZE 1280              /* Cells (4*bytes) */
  #define IMAGEDATASIZE 1280              /* Cells */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  object Workspace[WORKSPACESIZE] WORDALIGNED;
  
#elif defined(__MSP430FR5969__)
  #define WORKSPACESIZE 3072              /* Cells (4*bytes) */
  #define IMAGEDATASIZE 1536              /* Cells */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  object Workspace[WORKSPACESIZE] PERSIST WORDALIGNED;

#elif defined(__MSP430FR5994__)
  #define WORKSPACESIZE 3072              /* Cells (4*bytes) */
  #define IMAGEDATASIZE 1536              /* Cells */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  object Workspace[WORKSPACESIZE] PERSIST WORDALIGNED;

#elif defined(__MSP430FR6989__)
  #define WORKSPACESIZE 3072              /* Cells (4*bytes) */
  #define IMAGEDATASIZE 1536              /* Cells */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  object Workspace[WORKSPACESIZE] PERSIST WORDALIGNED;
  #include "LCD_Launchpad.h"
  LCD_LAUNCHPAD myLCD;
  
#endif

char SymbolTable[SYMBOLTABLESIZE];"#)

(defparameter *i2c-interface-msp430* #"
// I2C interface

uint8_t const TWI_SDA_PIN = 10;
uint8_t const TWI_SCL_PIN = 9;

void I2Cinit(bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

inline uint8_t I2Cread() {
  return Wire.read();
}

inline bool I2Cwrite(uint8_t data) {
  return Wire.write(data);
}

bool I2Cstart(uint8_t address, uint8_t read) {
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return true;
}

bool I2Crestart(uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(true) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return error ? false : true;
}

void I2Cstop(uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}"#)


(defparameter *stream-interface-msp430* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
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
  (void) address; (void) baud;
}

void serialend (int address) {
  (void) address;
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = istream(first(args));
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
  else error(PSTR("Unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
inline void serial1write (char c) { Serial1.write(c); }
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(__MSP430FR6989__)
inline void LCDwrite (char c) { myLCD.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = istream(first(args));
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
  #if defined(__MSP430FR6989__)
  else if (streamtype == LCDSTREAM) pfun = (pfun_t)LCDwrite;
  #endif
  else error(PSTR("unknown stream type"));
  return pfun;
}"#)


#+ignore
(defparameter *watchdog-msp430* #"
// Watchdog

void watchdogenable (int interval) {
  WDTCTL = 0x5a<<8 | ((interval == 0) ? 2 : 1);
}

void watchdogreset () {
  WDTCTL = 0x5a<<8 | 1<<WDTCNTCL | 1 ;
}"#)


(defparameter *check-pins-msp430* #"
// Check pins

void checkanalogread (int pin) {
#if defined(__MSP430F5529__)
  if (!(pin==2 || pin==6 || (pin>=23 && pin<=28))) error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR5969__)
  if (!(pin==5 || (pin>=11 && pin<=13) || pin==18 || pin==19 || pin==23 || pin==24)) error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR5994__)
  if (!(pin==2 || pin==6 || (pin>=23 && pin<=28) || (pin>=31 && pin<=33) || pin==43 || pin==44 || pin==47 || pin==134 || pin==135))
    error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR6989__)
  if (!(pin==2 || pin==6 || pin==17 || (pin>=23 && pin<=30) || pin==34 || (pin>=128 && pin<=130) || pin==143))
    error(PSTR("'analogread' invalid pin"));
#endif
}

void checkanalogwrite (int pin) {
#if defined(__MSP430F5529__)
  if (!(pin==12 || pin==19 || (pin>=35 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR5969__)
  if (!(pin==12 || pin==19 || (pin>=6 && pin<=15) || pin==18 || pin==19 || 
  pin==21 || pin==22 || pin==26)) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR5994__)
  if (!(pin==19 || (pin>=37 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR6989__)
  if (!(pin==19 || (pin>=37 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#endif
}"#)


(defparameter *note-msp430* #"
// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(PSTR("'note' octave out of range"));
  tone(pin, pgm_read_word(&scale[note%12])>>prescaler);
}

void nonote (int pin) {
  noTone(pin);
}"#)

(defparameter *sleep-msp430* #"
// Sleep

void initsleep () {
}

void sleep (int secs) {
  delay(1000*secs);
}"#)

