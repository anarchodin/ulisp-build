// Workspace - sizes in bytes
#define WORDALIGNED __attribute__((aligned (2)))
#define BUFFERSIZE 18

#if defined(__AVR_ATmega328P__)
  #define WORKSPACESIZE 315-SDSIZE        /* Objects (4*bytes) */
  #define EEPROMSIZE 1024                 /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 0

#elif defined(__AVR_ATmega2560__)
  #define WORKSPACESIZE 1214-SDSIZE       /* Objects (4*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define STACKDIFF 320

#elif defined(__AVR_ATmega1284P__)
  #define WORKSPACESIZE 2816-SDSIZE       /* Objects (4*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define STACKDIFF 320

#elif defined(ARDUINO_AVR_NANO_EVERY)
  #define WORKSPACESIZE 1066-SDSIZE       /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320

#elif defined(ARDUINO_AVR_ATmega4809)     /* Curiosity Nano using MegaCoreX */
  #define Serial Serial3
  #define WORKSPACESIZE 1066-SDSIZE       /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320

#elif defined(__AVR_ATmega4809__)
  #define WORKSPACESIZE 1066-SDSIZE       /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];
#define SDCARD_SS_PIN 10
