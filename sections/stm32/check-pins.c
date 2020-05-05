void checkanalogread (int pin) {
#if defined(_BOARD_MAPLE_MINI_H_)
  if (!(pin>=3 && pin<=11)) error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#elif defined(_VARIANT_ARDUINO_STM32_)
  if (!((pin>=0 && pin<=7) || pin==16)) error(ANALOGREAD, PSTR("invalid pin"), number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(_BOARD_MAPLE_MINI_H_)
  if (!((pin>=3 && pin<=5) || (pin>=8 && pin<=11) || (pin>=15 && pin<=16) || (pin>=25 && pin<=27)))
    error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#elif defined(_VARIANT_ARDUINO_STM32_)
  if (!((pin>=0 && pin<=3) || (pin>=6 && pin<=10) || pin==16 || (pin>=22 && pin<=23)))
    error(ANALOGWRITE, PSTR("invalid pin"), number(pin));
#endif
}
