;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

; Tiny Lisp Computer

(defparameter *compile-options-tlc* #"
// Compile options

#define checkoverflow
// #define resetautorun
// #define printfreespace
#define serialmonitor
#define tinylispcomputer"
"#)

(defparameter *workspace-tlc* #"
// Workspace - sizes in bytes
#define WORDALIGNED __attribute__((aligned (2)))
#define BUFFERSIZE 18

#if defined(__AVR_ATmega328P__)
#define WORKSPACESIZE 314-SDSIZE        /* Cells (4*bytes) */
#define IMAGEDATASIZE 254               /* Cells */
#define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */

#elif defined(__AVR_ATmega2560__)
#define WORKSPACESIZE 1216-SDSIZE       /* Cells (4*bytes) */
#define IMAGEDATASIZE 893               /* Cells */
#define SYMBOLTABLESIZE 512             /* Bytes */

#elif defined(__AVR_ATmega1284P__)
#define WORKSPACESIZE 2816-SDSIZE       /* Cells (4*bytes) */
#define IMAGEDATASIZE 893               /* Cells */
#define SYMBOLTABLESIZE 512             /* Bytes */
#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];
typedef int BitOrder;
#define SDCARD_SS_PIN 10
"#)

(defparameter *saveimage-tlc* #"
// Save-image and load-image

typedef struct {
  unsigned int eval;
  unsigned int datasize;
  unsigned int globalenv;
  unsigned int gcstack;
  #if SYMBOLTABLESIZE > BUFFERSIZE
  unsigned int symboltop;
  char table[SYMBOLTABLESIZE];
  #endif
  object data[IMAGEDATASIZE/4];
} struct_image;

struct_image EEMEM image;

int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
  // Save to EEPROM
  if (imagesize > IMAGEDATASIZE) {
    pfstring(PSTR("Error: image too large: "), pserial);
    pint(imagesize, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
  }
  eeprom_update_word(&image.datasize, imagesize);
  eeprom_update_word(&image.eval, (unsigned int)arg);
  eeprom_update_word(&image.globalenv, (unsigned int)GlobalEnv);
  eeprom_update_word(&image.gcstack, (unsigned int)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  eeprom_update_word(&image.symboltop, (unsigned int)SymbolTop);
  eeprom_update_block(SymbolTable, image.table, SYMBOLTABLESIZE);
  #endif
  eeprom_update_block(Workspace, image.data, imagesize*4);
  return imagesize;
}

int loadimage (object *filename) {
  (void) filename;
  unsigned int imagesize = eeprom_read_word(&image.datasize);
  if (imagesize == 0 || imagesize == 0xFFFF) error(PSTR("No saved image"));
  GlobalEnv = (object *)eeprom_read_word(&image.globalenv);
  GCStack = (object *)eeprom_read_word(&image.gcstack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)eeprom_read_word(&image.symboltop);
  eeprom_read_block(SymbolTable, image.table, SYMBOLTABLESIZE);
  #endif
  eeprom_read_block(Workspace, image.data, imagesize*4);
  gc(NULL, NULL);
  return imagesize;
}

void autorunimage () {
  object *nullenv = NULL;
  object *autorun = (object *)eeprom_read_word(&image.eval);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(autorun, NULL, &nullenv);
  }
}
"#)

(defparameter *i2c-interface-tlc* #"
// I2C interface

#if defined(__AVR_ATmega328P__)
uint8_t const TWI_SDA_PIN = 18;
uint8_t const TWI_SCL_PIN = 19;
#elif defined(__AVR_ATmega1280__) || defined(__AVR_ATmega2560__)
uint8_t const TWI_SDA_PIN = 20;
uint8_t const TWI_SCL_PIN = 21;
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
uint8_t const TWI_SDA_PIN = 17;
uint8_t const TWI_SCL_PIN = 16;
#elif defined(__AVR_ATmega32U4__)
uint8_t const TWI_SDA_PIN = 6;
uint8_t const TWI_SCL_PIN = 5;
#endif

uint32_t const F_TWI = 400000L;  // Hardware I2C clock in Hz
uint8_t const TWSR_MTX_DATA_ACK = 0x28;
uint8_t const TWSR_MTX_ADR_ACK = 0x18;
uint8_t const TWSR_MRX_ADR_ACK = 0x40;
uint8_t const TWSR_START = 0x08;
uint8_t const TWSR_REP_START = 0x10;
uint8_t const I2C_READ = 1;
uint8_t const I2C_WRITE = 0;

void I2Cinit(bool enablePullup) {
  TWSR = 0;                        // no prescaler
  TWBR = (F_CPU/F_TWI - 16)/2;     // set bit rate factor
  if (enablePullup) {
    digitalWrite(TWI_SDA_PIN, HIGH);
    digitalWrite(TWI_SCL_PIN, HIGH);
  }
}

uint8_t I2Cread() {
  if (I2CCount != 0) I2CCount--;
  TWCR = 1<<TWINT | 1<<TWEN | ((I2CCount == 0) ? 0 : (1<<TWEA));
  while (!(TWCR & 1<<TWINT));
  return TWDR;
}

bool I2Cwrite(uint8_t data) {
  TWDR = data;
  TWCR = 1<<TWINT | 1 << TWEN;
  while (!(TWCR & 1<<TWINT));
  return (TWSR & 0xF8) == TWSR_MTX_DATA_ACK;
}

bool I2Cstart(uint8_t address, uint8_t read) {
  uint8_t addressRW = address<<1 | read;
  TWCR = 1<<TWINT | 1<<TWSTA | 1<<TWEN;    // send START condition
  while (!(TWCR & 1<<TWINT));
  if ((TWSR & 0xF8) != TWSR_START && (TWSR & 0xF8) != TWSR_REP_START) return false;
  TWDR = addressRW;  // send device address and direction
  TWCR = 1<<TWINT | 1<<TWEN;
  while (!(TWCR & 1<<TWINT));
  if (addressRW & I2C_READ) return (TWSR & 0xF8) == TWSR_MRX_ADR_ACK;
  else return (TWSR & 0xF8) == TWSR_MTX_ADR_ACK;
}

bool I2Crestart(uint8_t address, uint8_t read) {
  return I2Cstart(address, read);
}

void I2Cstop(uint8_t read) {
  (void) read;
  TWCR = 1<<TWINT | 1<<TWEN | 1<<TWSTO;
  while (TWCR & 1<<TWSTO); // wait until stop and bus released
}
"#)

(defparameter *stream-interface-tlc* #"
// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(__AVR_ATmega1284P__)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#elif defined(__AVR_ATmega2560__)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
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
  #if defined(__AVR_ATmega328P__)
  (void) address; (void) baud;
  #elif defined(__AVR_ATmega1284P__)
  if (address == 1) Serial1.begin((long)baud*100);
  else error(PSTR("'with-serial' port not supported"));
  #elif defined(__AVR_ATmega2560__)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  else error(PSTR("'with-serial' port not supported"));
  #endif
}

void serialend (int address) {
  #if defined(__AVR_ATmega328P__)
  (void) address;
  #elif defined(__AVR_ATmega1284P__)
  if (address == 1) Serial1.end();
  #elif defined(__AVR_ATmega2560__)
  if (address == 1) Serial1.end();
  else if (address == 2) Serial2.end();
  else if (address == 3) Serial3.end();
  #endif
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
    #if defined(__AVR_ATmega1284P__)
    else if (address == 1) gfun = serial1read;
    #elif defined(__AVR_ATmega2560__)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error(PSTR("Unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(__AVR_ATmega1284P__)
inline void serial1write (char c) { Serial1.write(c); }
#elif defined(__AVR_ATmega2560__)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL) {
    int stream = istream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) pfun = spiwrite;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(__AVR_ATmega1284P__)
    else if (address == 1) pfun = serial1write;
    #elif defined(__AVR_ATmega2560__)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  else error(PSTR("unknown stream type"));
  return pfun;
}
"#)

(defparameter *check-pins-tlc* #"
// Check pins

void checkanalogread (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!(pin>=0 && pin<=5)) error(PSTR("'analogread' invalid pin"));
#elif defined(__AVR_ATmega2560__)
  if (!(pin>=0 && pin<=15)) error(PSTR("'analogread' invalid pin"));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin>=0 && pin<=7)) error(PSTR("'analogread' invalid pin"));
#endif
}

void checkanalogwrite (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!(pin>=3 && pin<=11 && pin!=4 && pin!=7 && pin!=8)) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__AVR_ATmega2560__)
  if (!((pin>=2 && pin<=13) || (pin>=44 && pin <=46))) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin==3 || pin==4 || pin==6 || pin==7 || (pin>=12 && pin<=15))) error(PSTR("'analogwrite' invalid pin"));
#endif
}
"#)

(defparameter *note-tlc* #"
// Note

const uint8_t scale[] PROGMEM = {239,226,213,201,190,179,169,160,151,142,134,127};

void playnote (int pin, int note, int octave) {
  #if defined(__AVR_ATmega328P__)
  if (pin == 3) {
    DDRD = DDRD | 1<<DDD3; // PD3 (Arduino D3) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 11) {
    DDRB = DDRB | 1<<DDB3; // PB3 (Arduino D11) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(PSTR("'note' pin not supported"));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(PSTR("'note' octave out of range"));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;

  #elif defined(__AVR_ATmega2560__)
  if (pin == 9) {
    DDRH = DDRH | 1<<DDH6; // PH6 (Arduino D9) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 10) {
    DDRB = DDRB | 1<<DDB4; // PB4 (Arduino D10) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(PSTR("'note' pin not supported"));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(PSTR("'note' octave out of range"));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;

  #elif defined(__AVR_ATmega1284P__)
  if (pin == 14) {
    DDRD = DDRD | 1<<DDD6; // PD6 (Arduino D14) as output
    TCCR2A = 0<<COM2A0 | 1<<COM2B0 | 2<<WGM20; // Toggle OC2B on match
  } else if (pin == 15) {
    DDRD = DDRD | 1<<DDD7; // PD7 (Arduino D15) as output
    TCCR2A = 1<<COM2A0 | 0<<COM2B0 | 2<<WGM20; // Toggle OC2A on match
  } else error(PSTR("'note' pin not supported"));
  int prescaler = 9 - octave - note/12;
  if (prescaler<3 || prescaler>6) error(PSTR("'note' octave out of range"));
  OCR2A = pgm_read_byte(&scale[note%12]) - 1;
  TCCR2B = 0<<WGM22 | prescaler<<CS20;
  #endif
}

void nonote (int pin) {
  (void) pin;
  TCCR2B = 0<<WGM22 | 0<<CS20;
}
"#)

(defparameter *print-functions1-tlc* #"
// Print functions

void pserial (char c) {
  LastPrint = c;
  Display(c);
  #if defined (serialmonitor)
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
  #endif
}
"#)

(defparameter *tiny-lisp-computer* #"
// Tiny Lisp Computer terminal and keyboard support

int const SH1106 = 0;     // Set to 0 for SSD1306 or 1 for SH1106

// Support both ATmega328P and ATmega644P/ATmega1284P
#if defined(__AVR_ATmega328P__)
#define PINX PIND
#define PORTDAT PORTB
int const data = 0;
#define KEYBOARD_VECTOR INT0_vect
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
#define PINX PINC
#define PORTDAT PORTC
int const data = 4;
#define KEYBOARD_VECTOR INT2_vect
#endif

// These are the bit positions in PORTX
int const clk = 7;
int const dc = 6;
int const cs = 5;

// Terminal **********************************************************************************

// Character set - stored in program memory
const uint8_t CharMap[96][6] PROGMEM = {
{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }, 
{ 0x00, 0x00, 0x5F, 0x00, 0x00, 0x00 }, 
{ 0x00, 0x07, 0x00, 0x07, 0x00, 0x00 }, 
{ 0x14, 0x7F, 0x14, 0x7F, 0x14, 0x00 }, 
{ 0x24, 0x2A, 0x7F, 0x2A, 0x12, 0x00 }, 
{ 0x23, 0x13, 0x08, 0x64, 0x62, 0x00 }, 
{ 0x36, 0x49, 0x56, 0x20, 0x50, 0x00 }, 
{ 0x00, 0x08, 0x07, 0x03, 0x00, 0x00 }, 
{ 0x00, 0x1C, 0x22, 0x41, 0x00, 0x00 }, 
{ 0x00, 0x41, 0x22, 0x1C, 0x00, 0x00 }, 
{ 0x2A, 0x1C, 0x7F, 0x1C, 0x2A, 0x00 }, 
{ 0x08, 0x08, 0x3E, 0x08, 0x08, 0x00 }, 
{ 0x00, 0x80, 0x70, 0x30, 0x00, 0x00 }, 
{ 0x08, 0x08, 0x08, 0x08, 0x08, 0x00 }, 
{ 0x00, 0x00, 0x60, 0x60, 0x00, 0x00 }, 
{ 0x20, 0x10, 0x08, 0x04, 0x02, 0x00 }, 
{ 0x3E, 0x51, 0x49, 0x45, 0x3E, 0x00 }, 
{ 0x00, 0x42, 0x7F, 0x40, 0x00, 0x00 }, 
{ 0x72, 0x49, 0x49, 0x49, 0x46, 0x00 }, 
{ 0x21, 0x41, 0x49, 0x4D, 0x33, 0x00 }, 
{ 0x18, 0x14, 0x12, 0x7F, 0x10, 0x00 }, 
{ 0x27, 0x45, 0x45, 0x45, 0x39, 0x00 }, 
{ 0x3C, 0x4A, 0x49, 0x49, 0x31, 0x00 }, 
{ 0x41, 0x21, 0x11, 0x09, 0x07, 0x00 }, 
{ 0x36, 0x49, 0x49, 0x49, 0x36, 0x00 }, 
{ 0x46, 0x49, 0x49, 0x29, 0x1E, 0x00 }, 
{ 0x00, 0x36, 0x36, 0x00, 0x00, 0x00 }, 
{ 0x00, 0x56, 0x36, 0x00, 0x00, 0x00 }, 
{ 0x00, 0x08, 0x14, 0x22, 0x41, 0x00 }, 
{ 0x14, 0x14, 0x14, 0x14, 0x14, 0x00 }, 
{ 0x00, 0x41, 0x22, 0x14, 0x08, 0x00 }, 
{ 0x02, 0x01, 0x59, 0x09, 0x06, 0x00 }, 
{ 0x3E, 0x41, 0x5D, 0x59, 0x4E, 0x00 }, 
{ 0x7C, 0x12, 0x11, 0x12, 0x7C, 0x00 }, 
{ 0x7F, 0x49, 0x49, 0x49, 0x36, 0x00 }, 
{ 0x3E, 0x41, 0x41, 0x41, 0x22, 0x00 }, 
{ 0x7F, 0x41, 0x41, 0x41, 0x3E, 0x00 }, 
{ 0x7F, 0x49, 0x49, 0x49, 0x41, 0x00 }, 
{ 0x7F, 0x09, 0x09, 0x09, 0x01, 0x00 }, 
{ 0x3E, 0x41, 0x41, 0x51, 0x73, 0x00 }, 
{ 0x7F, 0x08, 0x08, 0x08, 0x7F, 0x00 }, 
{ 0x00, 0x41, 0x7F, 0x41, 0x00, 0x00 }, 
{ 0x20, 0x40, 0x41, 0x3F, 0x01, 0x00 }, 
{ 0x7F, 0x08, 0x14, 0x22, 0x41, 0x00 }, 
{ 0x7F, 0x40, 0x40, 0x40, 0x40, 0x00 }, 
{ 0x7F, 0x02, 0x1C, 0x02, 0x7F, 0x00 }, 
{ 0x7F, 0x04, 0x08, 0x10, 0x7F, 0x00 }, 
{ 0x3E, 0x41, 0x41, 0x41, 0x3E, 0x00 }, 
{ 0x7F, 0x09, 0x09, 0x09, 0x06, 0x00 }, 
{ 0x3E, 0x41, 0x51, 0x21, 0x5E, 0x00 }, 
{ 0x7F, 0x09, 0x19, 0x29, 0x46, 0x00 }, 
{ 0x26, 0x49, 0x49, 0x49, 0x32, 0x00 }, 
{ 0x03, 0x01, 0x7F, 0x01, 0x03, 0x00 }, 
{ 0x3F, 0x40, 0x40, 0x40, 0x3F, 0x00 }, 
{ 0x1F, 0x20, 0x40, 0x20, 0x1F, 0x00 }, 
{ 0x3F, 0x40, 0x38, 0x40, 0x3F, 0x00 }, 
{ 0x63, 0x14, 0x08, 0x14, 0x63, 0x00 }, 
{ 0x03, 0x04, 0x78, 0x04, 0x03, 0x00 }, 
{ 0x61, 0x59, 0x49, 0x4D, 0x43, 0x00 }, 
{ 0x00, 0x7F, 0x41, 0x41, 0x41, 0x00 }, 
{ 0x02, 0x04, 0x08, 0x10, 0x20, 0x00 }, 
{ 0x00, 0x41, 0x41, 0x41, 0x7F, 0x00 }, 
{ 0x04, 0x02, 0x01, 0x02, 0x04, 0x00 }, 
{ 0x40, 0x40, 0x40, 0x40, 0x40, 0x00 }, 
{ 0x00, 0x03, 0x07, 0x08, 0x00, 0x00 }, 
{ 0x20, 0x54, 0x54, 0x78, 0x40, 0x00 }, 
{ 0x7F, 0x28, 0x44, 0x44, 0x38, 0x00 }, 
{ 0x38, 0x44, 0x44, 0x44, 0x28, 0x00 }, 
{ 0x38, 0x44, 0x44, 0x28, 0x7F, 0x00 }, 
{ 0x38, 0x54, 0x54, 0x54, 0x18, 0x00 }, 
{ 0x00, 0x08, 0x7E, 0x09, 0x02, 0x00 }, 
{ 0x18, 0xA4, 0xA4, 0x9C, 0x78, 0x00 }, 
{ 0x7F, 0x08, 0x04, 0x04, 0x78, 0x00 }, 
{ 0x00, 0x44, 0x7D, 0x40, 0x00, 0x00 }, 
{ 0x20, 0x40, 0x40, 0x3D, 0x00, 0x00 }, 
{ 0x7F, 0x10, 0x28, 0x44, 0x00, 0x00 }, 
{ 0x00, 0x41, 0x7F, 0x40, 0x00, 0x00 }, 
{ 0x7C, 0x04, 0x78, 0x04, 0x78, 0x00 }, 
{ 0x7C, 0x08, 0x04, 0x04, 0x78, 0x00 }, 
{ 0x38, 0x44, 0x44, 0x44, 0x38, 0x00 }, 
{ 0xFC, 0x18, 0x24, 0x24, 0x18, 0x00 }, 
{ 0x18, 0x24, 0x24, 0x18, 0xFC, 0x00 }, 
{ 0x7C, 0x08, 0x04, 0x04, 0x08, 0x00 }, 
{ 0x48, 0x54, 0x54, 0x54, 0x24, 0x00 }, 
{ 0x04, 0x04, 0x3F, 0x44, 0x24, 0x00 }, 
{ 0x3C, 0x40, 0x40, 0x20, 0x7C, 0x00 }, 
{ 0x1C, 0x20, 0x40, 0x20, 0x1C, 0x00 }, 
{ 0x3C, 0x40, 0x30, 0x40, 0x3C, 0x00 }, 
{ 0x44, 0x28, 0x10, 0x28, 0x44, 0x00 }, 
{ 0x4C, 0x90, 0x90, 0x90, 0x7C, 0x00 }, 
{ 0x44, 0x64, 0x54, 0x4C, 0x44, 0x00 }, 
{ 0x00, 0x08, 0x36, 0x41, 0x00, 0x00 }, 
{ 0x00, 0x00, 0x77, 0x00, 0x00, 0x00 }, 
{ 0x00, 0x41, 0x36, 0x08, 0x00, 0x00 }, 
{ 0x02, 0x01, 0x02, 0x04, 0x02, 0x00 }, 
{ 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0 }
};

// Initialisation sequence for OLED module
int const InitLen = 23;
unsigned char Init[InitLen] = {
  0xAE, // Display off
  0xD5, // Set display clock
  0x80, // Recommended value
  0xA8, // Set multiplex
  0x3F,
  0xD3, // Set display offset
  0x00,
  0x40, // Zero start line
  0x8D, // Charge pump
  0x14,
  0x20, // Memory mode
  0x02, // Page addressing
  0xA1, // 0xA0/0xA1 flip horizontally
  0xC8, // 0xC0/0xC8 flip vertically
  0xDA, // Set comp ins
  0x12,
  0x81, // Set contrast
  0x7F,
  0xD9, // Set pre charge
  0xF1,
  0xDB, // Set vcom detect
  0x40,
  0xA6  // Normal (0xA7=Inverse)
};

// Write a data byte to the display
void Data (uint8_t d) {  
  PINX = 1<<cs; // cs low
  for (uint8_t bit = 0x80; bit; bit >>= 1) {
    PINX = 1<<clk; // clk low
    if (d & bit) PORTDAT = PORTDAT | (1<<data); else PORTDAT = PORTDAT & ~(1<<data);
    PINX = 1<<clk; // clk high
  }
  PINX = 1<<cs; // cs high
}

// Write a command byte to the display
void Command (uint8_t c) { 
  PINX = 1<<dc; // dc low
  Data(c);
  PINX = 1<<dc; // dc high
}

void InitDisplay () {
  // Define pins
#if defined(__AVR_ATmega328P__)
  DDRD = DDRD | 1<<clk | 1<<dc | 1<<cs;     // All outputs
  PORTD = PORTD | 1<<clk | 1<<dc | 1<<cs;   // All high
  DDRB = DDRB | 1<<data;                    // Output
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
  DDRC = DDRC | 1<<clk | 1<<dc | 1<<cs;     // All outputs
  PORTC = PORTC | 1<<clk | 1<<dc | 1<<cs;   // All high
  DDRC = DDRC | 1<<data;                    // Output
#endif
  for (uint8_t c=0; c<InitLen; c++) Command(Init[c]);
  Display(12);    // Clear display
  Command(0xAF);  // Display on
}

// Character terminal

void ClearLine (uint8_t line) {
  Command(0xB0 + line);
  Command(0x00); // Column start low
  Command(0x00); // Column start high
  for (uint8_t b = 0 ; b < 128 + 4*SH1106; b++) Data(0);
}

// Clears the top line, then scrolls the display up by one line
void ScrollDisplay (uint8_t *scroll) {
  ClearLine(*scroll);
  *scroll = (*scroll + 1) & 0x07;
  Command(0xD3);
  Command(*scroll << 3);
}

// Plots a character; line = 0 to 7; column = 0 to 20
void PlotChar (char c, uint8_t line, uint8_t column) {
  column = column*6+2*SH1106;
  Command(0xB0 + (line & 0x07));
  Command(0x00 + (column & 0x0F)); // Column start low
  Command(0x10 + (column >> 4));   // Column start high
  for (uint8_t col = 0; col < 6; col++) {
    Data(pgm_read_byte(&CharMap[(c & 0x7F)-32][col]) ^ (c & 0x80 ? 0xFF : 0));
  }
}

// Prints a character to display, with cursor, handling control characters
void Display (char c) {
  static uint8_t Line = 0, Column = 0, Scroll = 0;
  // These characters don't affect the cursor
  if (c == 8) {                    // Backspace
    if (Column == 0) {
      Line--; Column = 20;
    } else Column--;
    return;
  }
  if (c == 9) {                    // Cursor forward
    if (Column == 20) {
      Line++; Column = 0;
    } else Column++;
    return;
  }
  if ((c >= 17) && (c <= 20)) {    // Parentheses
    if (c == 17) PlotChar('(', Line+Scroll, Column);
    else if (c == 18) PlotChar('(' | 0x80, Line+Scroll, Column);
    else if (c == 19) PlotChar(')', Line+Scroll, Column);
    else PlotChar(')' | 0x80, Line+Scroll, Column);
    return;
  }
  // Hide cursor
  PlotChar(' ', Line+Scroll, Column);
  if (c == 0x7F) {                 // DEL
    if (Column == 0) {
      Line--; Column = 20;
    } else Column--;
  } else if ((c & 0x7f) >= 32) {   // Normal character
    PlotChar(c, Line+Scroll, Column++);
    if (Column > 20) {
      Column = 0;
      if (Line == 7) ScrollDisplay(&Scroll); else Line++;
    }
  // Control characters
  } else if (c == 12) {            // Clear display
    for (uint8_t p=0; p < 8; p++) ClearLine(p);
    Line = 0; Column = 0;
  } else if (c == '\r') {            // Return
    Column = 0;
    if (Line == 7) ScrollDisplay(&Scroll); else Line++;
  } 
  // Show cursor
  PlotChar(0x7F, Line+Scroll, Column);
}

// Keyboard **********************************************************************************

const int KeymapSize = 132;
const int Cursor = 0x7F;

const char Keymap[] PROGMEM = 
// Without shift
"             \011`      q1   zsaw2  cxde43   vftr5  nbhgy6   mju78  ,kio09"
"  ./l;p-   \' [=    \015] \\        \010  1 47   0.2568\033  +3-*9      "
// With shift
"             \011~      Q!   ZSAW@  CXDE$#   VFTR%  NBHGY^   MJU&*  <KIO)("
"  >?L:P_   \" {+    \015} |        \010  1 47   0.2568\033  +3-*9       ";

// Parenthesis highlighting
void Highlight (uint8_t p, uint8_t invert) {
  if (p) {
    for (int n=0; n < p; n++) Display(8);
    Display(17 + invert);
    for (int n=1; n < p; n++) Display(9);
    Display(19 + invert);
    Display(9);
  }
} 
  
ISR(KEYBOARD_VECTOR) {
  static uint8_t Break = 0, Modifier = 0, Shift = 0, Parenthesis = 0;
  static int ScanCode = 0, ScanBit = 1;
#if defined(__AVR_ATmega328P__)
  if (PIND & 1<<PIND4) ScanCode = ScanCode | ScanBit;
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
  if (PINB & 1<<PINB1) ScanCode = ScanCode | ScanBit;
#endif  
  ScanBit = ScanBit << 1;
  if (ScanBit != 0x800) return;
  // Process scan code
  if ((ScanCode & 0x401) != 0x400) return; // Invalid start/stop bit
  int s = (ScanCode & 0x1FE) >> 1;
  ScanCode = 0, ScanBit = 1;
  if (s == 0xAA) return;                   // BAT completion code
  //
  if (s == 0xF0) { Break = 1; return; }
  if (s == 0xE0) { Modifier = 1; return; }
  if (Break) {
    if ((s == 0x12) || (s == 0x59)) Shift = 0;
    Break = 0; Modifier = 0; return;
  }
  if ((s == 0x12) || (s == 0x59)) Shift = 1;
  if (Modifier) return;
  char c = pgm_read_byte(&Keymap[s + KeymapSize*Shift]);
  if (c == 32 && s != 0x29) return;
  if (c == 27) { Escape = 1; return; }    // Escape key
  // Undo previous parenthesis highlight
  Highlight(Parenthesis, 0);
  Parenthesis = 0;  
  // Edit buffer
  if (c == '\r') {
    pchar('\r');
    KybdAvailable = 1;
    ReadPtr = 0;
    return;
  }
  if (c == 8) {     // Backspace key
    if (WritePtr > 0) {
      WritePtr--;
      Display(0x7F);
      if (WritePtr) c = KybdBuf[WritePtr-1];
    }
  } else if (WritePtr < KybdBufSize) {
    KybdBuf[WritePtr++] = c;
    Display(c);
  }
  // Do new parenthesis highlight
  if (c == ')') {
    int search = WritePtr-1, level = 0;
    while (search >= 0 && Parenthesis == 0) {
      c = KybdBuf[search--];
      if (c == ')') level++;
      if (c == '(') {
        level--;
        if (level == 0) Parenthesis = WritePtr-search-1;
      }
    }
    Highlight(Parenthesis, 1);
  }
  return;
}

void InitKybd() {
#if defined(__AVR_ATmega328P__)
  EICRA = 2<<ISC00;                       // Falling edge INT0
  PORTD = PORTD | 1<<PORTD4 | 1<<PORTD2;  // Enable pullups
  EIMSK = EIMSK | 1<<INT0;                // Enable interrupt
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
  EICRA = 2<<ISC20;                       // Falling edge INT2
  PORTB = PORTB | 1<<PORTB2 | 1<<PORTB1;  // Enable pullups
  EIMSK = EIMSK | 1<<INT2;                // Enable interrupt
#endif  
}

"#)


(defparameter *gchar-tlc* #"
volatile uint8_t WritePtr = 0, ReadPtr = 0;
const int KybdBufSize = 165;
char KybdBuf[KybdBufSize];
volatile uint8_t KybdAvailable = 0;

int gchar () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  #if defined (serialmonitor)
  while (!Serial.available() && !KybdAvailable);
  if (Serial.available()) {
    char temp = Serial.read();
    if (temp != '\r') pchar(temp);
    return temp;
  } else {
    if (ReadPtr != WritePtr) {
      char temp = KybdBuf[ReadPtr++];
      Serial.write(temp);
      return temp;
    }
    KybdAvailable = 0;
    WritePtr = 0;
    return 13;
  }
  #else
  while (!KybdAvailable);
  if (ReadPtr != WritePtr) return KybdBuf[ReadPtr++];
  KybdAvailable = 0;
  WritePtr = 0;
  return '\r';
  #elif defined (serialmonitor)
  while (!Serial.available());
  char temp = Serial.read();
  if (temp != '\r') pchar(temp);
  return temp;
  #endif
}
"#)

(defparameter *setup-tlc* #"
// Setup

void initenv() {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup() {
  InitDisplay();
  InitKybd();
  #if defined (serialmonitor)
  Serial.begin(9600);
  while (!Serial);  // wait for Serial to initialize
  #endif
  initworkspace();
  initenv();
  pfstring(PSTR("uLisp 2.0")), pserial); pln(pserial);
}
"#)
