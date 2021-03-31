// Platform specific settings

#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(ESP8266)
  #define WORKSPACESIZE (4000-SDSIZE)     /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define SDCARD_SS_PIN 10

#elif defined(ESP32)
  #define WORKSPACESIZE (8000-SDSIZE)     /* Cells (8*bytes) */
  #define EEPROMSIZE 4096                 /* Bytes available for EEPROM */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define analogWrite(x,y) dacWrite((x),(y))
  #define SDCARD_SS_PIN 13

#else
#error "Board not supported!"
#endif
