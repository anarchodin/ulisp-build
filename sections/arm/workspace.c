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
RAMFUNC uint8_t MyCode[CODESIZE] WORDALIGNED;
