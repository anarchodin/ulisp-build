// Check pins - these are board-specific not processor-specific

void checkanalogread (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!(pin>=0 && pin<=5)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega2560__)
  if (!(pin>=0 && pin<=15)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin>=0 && pin<=7)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_NANO_EVERY)
  if (!((pin>=14 && pin<=21))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_ATmega4809)  /* MegaCoreX core */
  if (!((pin>=22 && pin<=33) || (pin>=36 && pin<=39))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_ATmega4809__)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(__AVR_AVR128DA48__)
  if (!(pin>=22 && pin<=39)) error(ANALOGREAD, invalidpin, number(pin));
  #endif
}

void checkanalogwrite (int pin) {
#if defined(__AVR_ATmega328P__)
  if (!((pin>=2 && pin<=13) || pin==4 || pin==7 || pin==8)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega2560__)
  if (!((pin>=2 && pin<=13) || (pin>=44 && pin<=46))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega1284P__)
  if (!(pin==3 || pin==4 || pin==6 || pin==7 || (pin>=12 && pin<=15))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_NANO_EVERY)
  if (!(pin==3 || (pin>=5 && pin<=6) || (pin>=9 && pin<=11))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_AVR_ATmega4809)  /* MegaCoreX core */
  if (!((pin>=16 && pin<=19) || (pin>=38 && pin<=39))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_ATmega4809__)
  if (!(pin==3 || pin==5 || pin==6 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(__AVR_AVR128DA48__)
  if (!((pin>=4 && pin<=5) || (pin>=8 && pin<=19) || (pin>=38 && pin<=39))) error(ANALOGREAD, invalidpin, number(pin));
#endif
}
