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
char SymbolTable[SYMBOLTABLESIZE];
