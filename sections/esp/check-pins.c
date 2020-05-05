// Check pins

void checkanalogread (int pin) {
#if defined(ESP8266)
  if (pin!=17) error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#elif defined(ESP32)
  if (!(pin==0 || pin==2 || pin==4 || (pin>=12 && pin<=15) || (pin>=25 && pin<=27) || (pin>=32 && pin<=36) || pin==39))
    error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ESP8266)
  if (!(pin>=0 && pin<=16)) error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#elif defined(ESP32)
  if (!(pin>=25 && pin<=26)) error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#endif
}
