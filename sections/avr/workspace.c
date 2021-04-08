// Platform specific settings

#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 21                     /* longest builtin name + 1 */
#define RAMFUNC /* Ironically, will tag stuff that specifically _can't_ go in RAM. */

#if defined(__AVR_ATmega328P__)
  #define WORKSPACESIZE (314-SDSIZE)      /* Objects (4*bytes) */
  #define EEPROMSIZE 1024                 /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 0
  #define CPU_ATmega328P

#elif defined(__AVR_ATmega2560__)
  #define WORKSPACESIZE (1214-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define STACKDIFF 320
  #define CPU_ATmega2560

#elif defined(__AVR_ATmega1284P__)
  #include "optiboot.h"
  #define WORKSPACESIZE (2816-SDSIZE)     /* Objects (4*bytes) */
//  #define EEPROMSIZE 4096                 /* Bytes */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 256 */
  #define STACKDIFF 320
  #define CPU_ATmega1284P

#elif defined(ARDUINO_AVR_NANO_EVERY)
  #define WORKSPACESIZE (1060-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809
  
#elif defined(ARDUINO_AVR_ATmega4809)     /* Curiosity Nano using MegaCoreX */
  #define Serial Serial3
  #define WORKSPACESIZE (1065-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809

#elif defined(__AVR_ATmega4809__)
  #define WORKSPACESIZE (1065-SDSIZE)     /* Objects (4*bytes) */
  #define EEPROMSIZE 256                  /* Bytes */
  #define SYMBOLTABLESIZE BUFFERSIZE      /* Bytes - no long symbols */
  #define STACKDIFF 320
  #define CPU_ATmega4809

#elif defined(__AVR_AVR128DA48__)
  #include <Flash.h>
  #define Serial Serial1
  #define WORKSPACESIZE (2800-SDSIZE)     /* Objects (4*bytes) */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 480             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 512 */
  #define STACKDIFF 320
  #define CPU_AVR128DX48

#elif defined(__AVR_AVR128DB48__)
  #include <Flash.h>
  #define Serial Serial3
  #define WORKSPACESIZE (2750-SDSIZE)     /* Objects (4*bytes) */
  #define FLASHWRITESIZE 16384            /* Bytes */
  #define SYMBOLTABLESIZE 256             /* Bytes */
  #define CODESIZE 96                     /* Bytes <= 512 */
  #define STACKDIFF 320
  #define CPU_AVR128DX48

#else
#error "Board not supported!"
#endif
