// Check pins - these are board-specific not processor-specific

void checkanalogread (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!(pin>=54 && pin<=65)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!(pin>=15 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!(pin>=14 && pin<=25)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin>=8 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=67 && pin<=74) || (pin>=54 && pin<=61)))  error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==29)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin==1 || pin==2 || (pin>=4 && pin<=6) || pin==21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=14 && pin<=20)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!((pin>=0 && pin<=4) || pin==10 || pin==12 || pin==16)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!(pin>=49 && pin<=52)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=14 && pin<=27))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=14 && pin<=27) || (pin>=38 && pin<=41))) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!((pin>=2 && pin<=13) || pin==66 || pin==67)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || pin==14)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!((pin>=0 && pin<=8) || pin==10 || pin==18 || pin==19)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || (pin>=15 && pin<=16) || (pin>=22 && pin<=25))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin==0 || pin==2 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=0 && pin<=15)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin==0 || pin==1 || pin==4 || pin==5 || pin==7 || (pin>=9 && pin<=15) || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin==0 || pin==1 || (pin>=4 && pin<=6) || (pin>=9 && pin<=13) || pin==14 || pin==15 || pin==17 || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=2 && pin<=9) || pin==11 || (pin>=13 && pin<=45) || pin==48 || (pin>=50 && pin<=53) || pin==58 || pin==61 || pin==68 || pin==69)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_BBC_MICROBIT_V2)
  if (!(pin>=0 && pin<=32)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_CALLIOPE_MINI)
  if (!(pin>=0 && pin<=30)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SINOBIT)
  if (!(pin>=0 && pin<=32)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_ITSYBITSY)
  if (!(pin>=0 && pin<=25)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_NRF52840_CLUE)
  if (!(pin>=0 && pin<=46)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!((pin>=20 && pin<=29) || pin==32 || (pin>=40 && pin<=48))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY40)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || (pin>=33 && pin<=39))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_TEENSY41)
  if (!((pin>=0 && pin<=15) || (pin>=18 && pin<=19) || (pin>=22 && pin<=25) || (pin>=28 && pin<=29) || pin==33 || (pin>=36 && pin<=37))) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}
