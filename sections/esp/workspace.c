// Workspace
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(ESP8266)
  #define PSTR(s) s
  #define PROGMEM
  #define WORKSPACESIZE 3072-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define SDCARD_SS_PIN 10
  uint8_t _end;
  typedef int BitOrder;

#elif defined(ESP32)
  #define WORKSPACESIZE 8000-SDSIZE       /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13
  uint8_t _end;
  typedef int BitOrder;

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];
