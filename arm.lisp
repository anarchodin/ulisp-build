;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

; Arm

(defparameter *header-arm*
#"/* uLisp ARM 3.2 - www.ulisp.com
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
#endif"#)

(defparameter *workspace-arm* #"
// Workspace
#define PERSIST __attribute__((section(".text")))
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2
#define RAMFUNC __attribute__ ((section (".ramfunctions")))

#if defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
  #define WORKSPACESIZE 2816-SDSIZE       /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 4
  #define STACKDIFF 320

#elif defined(ARDUINO_GEMMA_M0)
  #define WORKSPACESIZE 2816-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 320

#elif defined(ARDUINO_METRO_M4) || defined(ARDUINO_ITSYBITSY_M4) || defined(ARDUINO_FEATHER_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
  #define WORKSPACESIZE 20480-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 400

#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  #define WORKSPACESIZE 28672-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 8192000           /* 8 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 400

#elif defined(ARDUINO_SAMD_MKRZERO)
  #define WORKSPACESIZE 2816-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define STACKDIFF 840

#elif defined(ARDUINO_SAMD_ZERO)          /* Put this last, otherwise overrides the Adafruit boards */
  #define WORKSPACESIZE 2816-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 128                    /* Bytes */
  #define SDCARD_SS_PIN 10
  #define STACKDIFF 320

#elif defined(_VARIANT_BBC_MICROBIT_)
  #define WORKSPACESIZE 1280              /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 64                     /* Bytes */
  #define STACKDIFF 320

#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  #define WORKSPACESIZE 20992-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 1200
  
#elif defined(ARDUINO_NRF52840_CLUE)
  #define WORKSPACESIZE 19456-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 0

#elif defined(MAX32620)
  #define WORKSPACESIZE 24576-SDSIZE      /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320

#elif defined(ARDUINO_FEATHER_F405)
  #define WORKSPACESIZE 11840-SDSIZE      /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 256                    /* Bytes */
  #define STACKDIFF 320

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];
RAMFUNC uint8_t MyCode[CODESIZE] WORDALIGNED;"#)

(defparameter *stream-interface-arm* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
inline int spi1read () { return SPI1.transfer(0); }
#endif
#if defined(ARDUINO_SAM_DUE)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
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
  #if defined(_VARIANT_BBC_MICROBIT_) || defined(ARDUINO_FEATHER_F405)
  error(WITHSERIAL, PSTR("port not supported"), number(address));
  #elif defined(ARDUINO_SAM_DUE)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #else
  if (address == 1) Serial1.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #endif
}

void serialend (int address) {
  #if defined(ARDUINO_SAM_DUE)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
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
  else if (streamtype == SPISTREAM) {
    if (address < 128) gfun = spiread;
    #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
    else gfun = spi1read;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
  #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
    else if (address == 1) gfun = serial1read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
inline void spi1write (char c) { SPI1.transfer(c); }
#endif
#if defined(ARDUINO_SAM_DUE)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#elif !defined(_VARIANT_BBC_MICROBIT_)
inline void serial1write (char c) { Serial1.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(gfxsupport)
inline void gfxwrite (char c) { tft.write(c); }
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
  else if (streamtype == SPISTREAM) {
    if (address < 128) pfun = spiwrite;
    #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
    else pfun = spi1write;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
    else if (address == 1) pfun = serial1write;
    #endif
  }   
  else if (streamtype == STRINGSTREAM) {
    pfun = pstr;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  #if defined(gfxsupport)
  else if (streamtype == GFXSTREAM) pfun = (pfun_t)gfxwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}"#)


(defparameter *check-pins-arm* #"
// Check pins

void checkanalogread (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!(pin>=54 && pin<=65)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!(pin>=15 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!(pin>=14 && pin<=25)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin>=8 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=67 && pin<=74) || (pin>=54 && pin<=61))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(_VARIANT_BBC_MICROBIT_)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==12 || pin==16)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!(pin>=49 && pin<=52)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!((pin>=2 && pin<=13) || pin==66 || pin==67)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || pin==14)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!((pin>=0 && pin<=8) || pin==10 || pin==18 || pin==19)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || (pin>=15 && pin<=16) || (pin>=22 && pin<=25))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin==0 || pin==2 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=0 && pin<=15)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin==0 || pin==1 || pin==4 || pin==5 || pin==7 || (pin>=9 && pin<=15) || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin==0 || pin==1 || (pin>=4 && pin<=6) || (pin>=9 && pin<=13) || pin==14 || pin==15 || pin==17 || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=2 && pin<=9) || pin==11 || (pin>=13 && pin<=45) || pin==48 || (pin>=50 && pin<=53) || pin==58 || pin==61 || pin==68 || pin==69)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(_VARIANT_BBC_MICROBIT_)
  if (!(pin>=0 && pin<=2)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=0 && pin<=25)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!(pin>=0 && pin<=46)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!((pin>=20 && pin<=29) || pin==32 || (pin>=40 && pin<=48))) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}"#)

(defparameter *note-arm* #"
// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(ARDUINO_NRF52840_CLUE)
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
#endif
}

void nonote (int pin) {
#if defined(ARDUINO_NRF52840_CLUE)
  noTone(pin);
#endif
}"#)

(defparameter *sleep-arm* #"
// Sleep

#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
void WDT_Handler(void) {
  // ISR for watchdog early warning
  WDT->CTRL.bit.ENABLE = 0;        // Disable watchdog
  while(WDT->STATUS.bit.SYNCBUSY); // Sync CTRL write
  WDT->INTFLAG.bit.EW  = 1;        // Clear interrupt flag
}
#endif

void initsleep () {
#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
 // One-time initialization of watchdog timer.

  // Generic clock generator 2, divisor = 32 (2^(DIV+1))
  GCLK->GENDIV.reg = GCLK_GENDIV_ID(2) | GCLK_GENDIV_DIV(4);
  // Enable clock generator 2 using low-power 32KHz oscillator.
  // With /32 divisor above, this yields 1024Hz clock.
  GCLK->GENCTRL.reg = GCLK_GENCTRL_ID(2) |
                      GCLK_GENCTRL_GENEN |
                      GCLK_GENCTRL_SRC_OSCULP32K |
                      GCLK_GENCTRL_DIVSEL;
  while(GCLK->STATUS.bit.SYNCBUSY);
  // WDT clock = clock gen 2
  GCLK->CLKCTRL.reg = GCLK_CLKCTRL_ID_WDT |
                      GCLK_CLKCTRL_CLKEN |
                      GCLK_CLKCTRL_GEN_GCLK2;

  // Enable WDT early-warning interrupt
  NVIC_DisableIRQ(WDT_IRQn);
  NVIC_ClearPendingIRQ(WDT_IRQn);
  NVIC_SetPriority(WDT_IRQn, 0);         // Top priority
  NVIC_EnableIRQ(WDT_IRQn);
#endif
}

void sleep (int secs) {
#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
  WDT->CTRL.reg = 0;                     // Disable watchdog for config
  while(WDT->STATUS.bit.SYNCBUSY);
  WDT->INTENSET.bit.EW   = 1;            // Enable early warning interrupt
  WDT->CONFIG.bit.PER    = 0xB;          // Period = max
  WDT->CONFIG.bit.WINDOW = 0x7;          // Set time of interrupt = 1024 cycles = 1 sec
  WDT->CTRL.bit.WEN      = 1;            // Enable window mode
  while(WDT->STATUS.bit.SYNCBUSY);       // Sync CTRL write

  SysTick->CTRL = 0;                     // Stop SysTick interrupts
  
  while (secs > 0) {
    WDT->CLEAR.reg = WDT_CLEAR_CLEAR_KEY;// Clear watchdog interval
    while(WDT->STATUS.bit.SYNCBUSY);
    WDT->CTRL.bit.ENABLE = 1;            // Start watchdog now!
    while(WDT->STATUS.bit.SYNCBUSY);
    SCB->SCR |= SCB_SCR_SLEEPDEEP_Msk;   // Deepest sleep
    __DSB();
    __WFI();                             // Wait for interrupt
    secs--;
  }
  SysTick->CTRL = 7;                     // Restart SysTick interrupts
#else
  delay(1000*secs);
#endif
}"#)
