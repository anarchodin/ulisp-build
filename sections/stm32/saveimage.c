// Save-image and load-image
const unsigned int Eeprom = 0x801D800;

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}
#else
void FlashSetup () {
  FLASH_Unlock();
  uint16_t Status;
  for (int page = Eeprom; page < 0x8020000; page = page + 0x400) {
    Status = FLASH_ErasePage(page);
    if (Status != FLASH_COMPLETE) error2(SAVEIMAGE, PSTR("flash erase failed"));
  }
}

void FlashWrite16 (unsigned int *addr, uint16_t data) {
  uint16_t Status = FLASH_ProgramHalfWord((*addr) + Eeprom, data);
  if (Status != FLASH_COMPLETE) error2(SAVEIMAGE, PSTR("flash write failed"));
  (*addr) = (*addr) + 2;
}

void FlashWriteInt (unsigned int *addr, int data) {
  FlashWrite16(addr, data & 0xFFFF); FlashWrite16(addr, data>>16 & 0xFFFF);
}
#endif

int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error3(SAVEIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#else
  FlashSetup();
  // Save to EEPROM
  int bytesneeded = imagesize*8 + SYMBOLTABLESIZE + 20;
  if (bytesneeded > EEPROMSIZE) {
    pfstring(PSTR("Error: Image size too large: "), pserial);
    pint(imagesize, pserial); pln(pserial);
    GCStack = NULL;
    longjmp(exception, 1);
  }
  unsigned int addr = 0;
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i=i+2) FlashWrite16(&addr, SymbolTable[i] | SymbolTable[i+1]<<8);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  return imagesize; 
}
#endif

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#else
uint16_t FlashRead16 (unsigned int *addr) {
  uint16_t data = (*(__IO uint16*)((*addr) + Eeprom));
  (*addr) = (*addr) + 2;
  return data;
}

int FlashReadInt (unsigned int *addr) {
  uint16_t b0 = FlashRead16(addr);
  uint16_t b1 = FlashRead16(addr);
  return b0 | b1<<16;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error3(LOADIMAGE, PSTR("illegal argument"));
  if (!file) error(PSTR("Problem loading from SD card"));
  SDReadInt(file);
  int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
  #endif
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#else
  unsigned int addr = 0;
  FlashReadInt(&addr); // Skip eval address
  int imagesize = FlashReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt(&addr);
  GCStack = (object *)FlashReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt(&addr);
  for (int i=0; i<SYMBOLTABLESIZE; i=i+2) {
    uint16_t bytes = FlashRead16(&addr);
    SymbolTable[i] = bytes & 0xFF;
    SymbolTable[i+1] = bytes>>8 & 0xFF;
  }
  #endif
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt(&addr);
    cdr(obj) = (object *)FlashReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error(PSTR("Error: Problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#else
  unsigned int addr = 0;
  object *autorun = (object *)FlashReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#endif
}
