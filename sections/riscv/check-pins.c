// Check pins

void checkanalogread (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!((pin>=32 && pin<=36) || pin==39)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  if (!(pin>=0 && pin<=13)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_BIT)
  if (!(pin>=0 && pin<=35)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(BOARD_SIPEED_MAIX_ONE_DOCK)
  if (!(pin>=0 && pin<=47)) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}
