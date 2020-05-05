// Streams

inline int spiread () { return SPI.transfer(0); }
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return SDgfile.read();
}
#endif

void serialbegin (int address, int baud) {
  (void) address; (void) baud;
}

void serialend (int address) {
  (void) address;
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = istream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
  else if (streamtype == SPISTREAM) gfun = spiread;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    else if (address == 1) gfun = serial1read;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error(PSTR("Unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
inline void serial1write (char c) { Serial1.write(c); }
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(__MSP430FR6989__)
inline void LCDwrite (char c) { myLCD.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = istream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) pfun = spiwrite;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    else if (address == 1) pfun = serial1write;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  #if defined(__MSP430FR6989__)
  else if (streamtype == LCDSTREAM) pfun = (pfun_t)LCDwrite;
  #endif
  else error(PSTR("unknown stream type"));
  return pfun;
}
