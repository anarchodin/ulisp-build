// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}
#elif defined(DATAFLASHSIZE)
// Winbond DataFlash support for Adafruit M4 Express boards
#define PAGEPROG      0x02
#define READSTATUS    0x05
#define READDATA      0x03
#define WRITEENABLE   0x06
#define BLOCK64K      0xD8
#define READID        0x90

// Arduino pins used for dataflash
#if defined(ARDUINO_ITSYBITSY_M0) || defined(ARDUINO_SAMD_FEATHER_M0_EXPRESS)
const int sck = 38, ssel = 39, mosi = 37, miso = 36;
#elif defined(EXTERNAL_FLASH_USE_QSPI)
const int sck = PIN_QSPI_SCK, ssel = PIN_QSPI_CS, mosi = PIN_QSPI_IO0, miso = PIN_QSPI_IO1;
#endif

boolean FlashSetup () {
  uint8_t manID, devID;
  digitalWrite(ssel, HIGH); pinMode(ssel, OUTPUT);
  pinMode(sck, OUTPUT);
  pinMode(mosi, OUTPUT);
  pinMode(miso, INPUT);
  digitalWrite(sck, LOW); digitalWrite(mosi, HIGH);
  digitalWrite(ssel, LOW);
  FlashWrite(READID);
  for(uint8_t i=0; i<4; i++) manID = FlashRead();
  devID = FlashRead();
  digitalWrite(ssel, HIGH);
  return (devID == 0x14 || devID == 0x15 || devID == 0x16); // Found correct device
}

inline void FlashWrite (uint8_t data) {
  shiftOut(mosi, sck, MSBFIRST, data);
}

void FlashBusy () {
  digitalWrite(ssel, 0);
  FlashWrite(READSTATUS);
  while (FlashRead() & 1 != 0);
  digitalWrite(ssel, 1);
}

void FlashWriteEnable () {
  digitalWrite(ssel, 0);
  FlashWrite(WRITEENABLE);
  digitalWrite(ssel, 1);
}

void FlashBeginRead () {
  FlashBusy();
  digitalWrite(ssel, 0);
  FlashWrite(READDATA);
  FlashWrite(0); FlashWrite(0); FlashWrite(0);
}

inline uint8_t FlashRead () {
  int data;
  return shiftIn(miso, sck, MSBFIRST);
}

inline void FlashEndRead(void) {
  digitalWrite(ssel, 1);
}

void FlashBeginWrite (int blocks) {
  // Erase 64K
  for (int b=0; b<blocks; b++) {
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(BLOCK64K);
    FlashWrite(b); FlashWrite(0); FlashWrite(0);
    digitalWrite(ssel, 1);
    FlashBusy();
  }
}

inline uint8_t FlashReadByte () {
  return FlashRead();
}

void FlashWriteByte (uint32_t *addr, uint8_t data) {
  // New page
  if (((*addr) & 0xFF) == 0) {
    digitalWrite(ssel, 1);
    FlashBusy();
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(PAGEPROG);
    FlashWrite((*addr)>>16);
    FlashWrite((*addr)>>8);
    FlashWrite(0);
  }
  FlashWrite(data);
  (*addr)++;
}

inline void FlashEndWrite (void) {
  digitalWrite(ssel, 1);
  FlashBusy();
}

void FlashWriteInt (uint32_t *addr, int data) {
  FlashWriteByte(addr, data & 0xFF); FlashWriteByte(addr, data>>8 & 0xFF);
  FlashWriteByte(addr, data>>16 & 0xFF); FlashWriteByte(addr, data>>24 & 0xFF);
}
#endif

int saveimage (object *arg) {
#if defined(sdcardsupport)
  unsigned int imagesize = compactimage(&arg);
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error(SAVEIMAGE, invalidarg, arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) file.write(SymbolTable[i]);
  #endif
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(DATAFLASHSIZE)
  unsigned int imagesize = compactimage(&arg);
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (!FlashSetup()) error2(SAVEIMAGE, PSTR("no DataFlash found."));
  // Save to DataFlash
  int SymbolUsed = SymbolTop - SymbolTable;
  int bytesneeded = 20 + SymbolUsed + CODESIZE + imagesize*8;
  if (bytesneeded > DATAFLASHSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr = 0;
  FlashBeginWrite((bytesneeded+65535)/65536);
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SymbolUsed; i++) FlashWriteByte(&addr, SymbolTable[i]);
  #endif
  for (int i=0; i<CODESIZE; i++) FlashWriteByte(&addr, MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite();
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#elif defined(DATAFLASHSIZE)
int FlashReadInt () {
  uint8_t b0 = FlashReadByte(); uint8_t b1 = FlashReadByte();
  uint8_t b2 = FlashReadByte(); uint8_t b3 = FlashReadByte();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error(LOADIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  unsigned int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = file.read();
  #endif
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(DATAFLASHSIZE)
  if (!FlashSetup()) error2(LOADIMAGE, PSTR("no DataFlash found."));
  FlashBeginRead();
  FlashReadInt(); // Skip eval address
  unsigned int imagesize = FlashReadInt();
  if (imagesize == 0 || imagesize == 0xFFFFFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt();
  GCStack = (object *)FlashReadInt();
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt();
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = FlashReadByte();
  #endif
  for (int i=0; i<CODESIZE; i++) MyCode[i] = FlashReadByte();
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt();
    cdr(obj) = (object *)FlashReadInt();
  }
  gc(NULL, NULL);
  FlashEndRead();
  return imagesize;
#else
  (void) arg;
  error2(LOADIMAGE, PSTR("not available"));
  return 0;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(0, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#elif defined(DATAFLASHSIZE)
  if (!FlashSetup()) error2(0, PSTR("no DataFlash found."));
  FlashBeginRead();
  object *autorun = (object *)FlashReadInt();
  FlashEndRead();
  if (autorun != NULL && (unsigned int)autorun != 0xFFFFFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#else
  error2(0, PSTR("autorun not available"));
#endif
}
