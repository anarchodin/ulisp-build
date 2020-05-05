// Save-image and load-image

typedef union {
  uintptr_t sdpointer;
  uint8_t sdbyte[8];
} sdbuffer_t;

#if defined(sdcardsupport)
void SDWriteInt (File file, uintptr_t data) {
  sdbuffer_t sdbuf;
  sdbuf.sdpointer = data;
  for (int i=0; i<8; i++) file.write(sdbuf.sdbyte[i]);
}
#endif

int saveimage (object *arg) {
  uintptr_t imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  if (!SD.begin(SDCARD_SS_PIN)) error2(SAVEIMAGE, PSTR("problem initialising SD card"));
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), FILE_WRITE);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error(SAVEIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, (uintptr_t)imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
  #endif
  for (int i=0; i<CODESIZE; i++) file.write(MyCode[i]);
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

#if defined(sdcardsupport)
uintptr_t SDReadInt (File file) {
  sdbuffer_t sdbuf;
  for (int i=0; i<8; i++) sdbuf.sdbyte[i] = file.read();
  return sdbuf.sdpointer;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  if (!SD.begin(SDCARD_SS_PIN)) error2(LOADIMAGE, PSTR("problem initialising SD card"));
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("ULISP.IMG");
  else error(LOADIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  uintptr_t imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
  #endif
  for (int i=0; i<CODESIZE; i++) MyCode[i] = file.read();
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
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
#else
  error2(0, PSTR("autorun not available"));
#endif
}
