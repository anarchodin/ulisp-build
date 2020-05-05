// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
inline int spi1read () { return SPI1.transfer(0); }
#endif
#if defined(ARDUINO_SAM_DUE)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#endif
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
  #if defined(_VARIANT_BBC_MICROBIT_) || defined(ARDUINO_FEATHER_F405)
  error(WITHSERIAL, PSTR("port not supported"), number(address));
  #elif defined(ARDUINO_SAM_DUE)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #else
  if (address == 1) Serial1.begin((long)baud*100);
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
  #endif
}

void serialend (int address) {
  #if defined(ARDUINO_SAM_DUE)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #endif
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
  else if (streamtype == SPISTREAM) {
    if (address < 128) gfun = spiread;
    #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
    else gfun = spi1read;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
  #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
    else if (address == 1) gfun = serial1read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
inline void spi1write (char c) { SPI1.transfer(c); }
#endif
#if defined(ARDUINO_SAM_DUE)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#elif !defined(_VARIANT_BBC_MICROBIT_)
inline void serial1write (char c) { Serial1.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif
#if defined(gfxsupport)
inline void gfxwrite (char c) { tft.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) {
    if (address < 128) pfun = spiwrite;
    #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
    else pfun = spi1write;
    #endif
  }
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #elif !defined(_VARIANT_BBC_MICROBIT_) && !defined(ARDUINO_FEATHER_F405)
    else if (address == 1) pfun = serial1write;
    #endif
  }   
  else if (streamtype == STRINGSTREAM) {
    pfun = pstr;
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  #if defined(gfxsupport)
  else if (streamtype == GFXSTREAM) pfun = (pfun_t)gfxwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}
