// Workspace
#define WORDALIGNED __attribute__((aligned (8)))
#define BUFFERSIZE 34  // Number of bits+2
#define RAMFUNC __attribute__ ((section (".ramfunctions")))

#if defined(BOARD_SIPEED_MAIX_DUINO)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096

#elif defined(BOARD_SIPEED_MAIX_BIT)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096

#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  #define WORKSPACESIZE 80000             /* Objects (16*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define CODESIZE 512                    /* Bytes */
  #define SDCARD_SS_PIN 29
  #define STACKDIFF 4096

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];
uint8_t MyCode[CODESIZE] WORDALIGNED;
