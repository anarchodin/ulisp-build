// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
#endif
}

void nonote (int pin) {
#if defined(BOARD_SIPEED_MAIX_DUINO)
  noTone(pin);
#endif
}
