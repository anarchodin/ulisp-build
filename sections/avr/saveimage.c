// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
}
#elif defined(FLASHWRITESIZE)
#if defined (CPU_ATmega1284P)
// save-image area is the 16K bytes (64 256-byte pages) from 0x1bc00 to 0x1fc00
const uint32_t BaseAddress = 0x1bc00;
uint8_t FlashCheck() {
  return 0;
}

void FlashWriteInt (uint32_t *addr, int data) {
  if (((*addr) & 0xFF) == 0) optiboot_page_erase(BaseAddress + ((*addr) & 0xFF00));
  optiboot_page_fill(BaseAddress + *addr, data);
  if (((*addr) & 0xFF) == 0xFE) optiboot_page_write(BaseAddress + ((*addr) & 0xFF00));
  (*addr)++; (*addr)++;
}

void FlashEndWrite (uint32_t *addr) {
  if (((*addr) & 0xFF) != 0) optiboot_page_write((BaseAddress + ((*addr) & 0xFF00)));
}

#elif defined (CPU_AVR128DX48)
// save-image area is the 16K bytes (32 512-byte pages) from 0x1c000 to 0x20000
const uint32_t BaseAddress = 0x1c000;
uint8_t FlashCheck() {
  return Flash.checkWritable();
}

void FlashWriteInt (uint32_t *addr, int data) {
  if (((*addr) & 0x1FF) == 0) Flash.erasePage(BaseAddress + ((*addr) & 0xFE00));
  Flash.writeWord(BaseAddress + *addr, data);
  (*addr)++; (*addr)++;
}

void FlashEndWrite (uint32_t *addr) {
}
#endif
#else
void EEPROMWriteInt (unsigned int *addr, int data) {
  EEPROM.write((*addr)++, data & 0xFF); EEPROM.write((*addr)++, data>>8 & 0xFF);
}
#endif

unsigned int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("/ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
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
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  // Save to Flash
  int SymbolUsed = SymbolTop - SymbolTable;
  int bytesneeded = 10 + SYMBOLTABLESIZE + CODESIZE + imagesize*4;
  if (bytesneeded > FLASHWRITESIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  uint32_t addr = 0;
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE/2; i++) FlashWriteInt(&addr, SymbolTable[i*2] | SymbolTable[i*2+1]<<8);
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE/2; i++) FlashWriteInt(&addr, MyCode[i*2] | MyCode[i*2+1]<<8);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite(&addr);
  return imagesize;
#else
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, invalidarg, arg);
  int SymbolUsed = SymbolTop - SymbolTable;
  int bytesneeded = imagesize*4 + SymbolUsed + 10;
  if (bytesneeded > EEPROMSIZE) error(SAVEIMAGE, PSTR("image too large"), number(imagesize));
  unsigned int addr = 0;
  EEPROMWriteInt(&addr, (unsigned int)arg);
  EEPROMWriteInt(&addr, imagesize);
  EEPROMWriteInt(&addr, (unsigned int)GlobalEnv);
  EEPROMWriteInt(&addr, (unsigned int)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  EEPROMWriteInt(&addr, (unsigned int)SymbolTop);
  for (int i=0; i<SymbolUsed; i++) EEPROM.write(addr++, SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    EEPROMWriteInt(&addr, (uintptr_t)car(obj));
    EEPROMWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  return imagesize;
#endif
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uint8_t b0 = file.read(); uint8_t b1 = file.read();
  return b0 | b1<<8;
}
#elif defined(FLASHWRITESIZE)
#if defined (CPU_ATmega1284P)
uint8_t FlashReadByte (uint32_t *addr) {
  return pgm_read_byte_far(BaseAddress + (*addr)++);
}

int FlashReadInt (uint32_t *addr) {
  int data = pgm_read_word_far(BaseAddress + *addr);
  (*addr)++; (*addr)++;
  return data;
}
#elif defined (CPU_AVR128DX48)
uint8_t FlashReadByte (uint32_t *addr) {
  return Flash.readByte(BaseAddress + (*addr)++);
}

int FlashReadInt (uint32_t *addr) {
  int data = Flash.readWord(BaseAddress + *addr);
  (*addr)++; (*addr)++;
  return data;
}
#endif
#else
int EEPROMReadInt (unsigned int *addr) {
  uint8_t b0 = EEPROM.read((*addr)++); uint8_t b1 = EEPROM.read((*addr)++);
  return b0 | b1<<8;
}
#endif

unsigned int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error(LOADIMAGE, invalidarg, arg);
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
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(FLASHWRITESIZE)
  if (FlashCheck()) error2(SAVEIMAGE, PSTR("flash write not supported"));
  uint32_t addr = 0;
  FlashReadInt(&addr); // Skip eval address
  unsigned int imagesize = FlashReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt(&addr);
  GCStack = (object *)FlashReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt(&addr);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = FlashReadByte(&addr);
  #endif
  #if defined(CODESIZE)
  for (int i=0; i<CODESIZE; i++) MyCode[i] = FlashReadByte(&addr);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt(&addr);
    cdr(obj) = (object *)FlashReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
#else
  unsigned int addr = 2; // Skip eval address
  unsigned int imagesize = EEPROMReadInt(&addr);
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)EEPROMReadInt(&addr);
  GCStack = (object *)EEPROMReadInt(&addr);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)EEPROMReadInt(&addr);
  int SymbolUsed = SymbolTop - SymbolTable;
  for (int i=0; i<SymbolUsed; i++) SymbolTable[i] = EEPROM.read(addr++);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)EEPROMReadInt(&addr);
    cdr(obj) = (object *)EEPROMReadInt(&addr);
  }
  gc(NULL, NULL);
  return imagesize;
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
#elif defined(FLASHWRITESIZE)
  uint32_t addr = 0;
  object *autorun = (object *)FlashReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#else
  unsigned int addr = 0;
  object *autorun = (object *)EEPROMReadInt(&addr);
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#endif
}
