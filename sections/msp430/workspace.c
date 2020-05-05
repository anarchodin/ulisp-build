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

char SymbolTable[SYMBOLTABLESIZE];
