// Note

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(PSTR("'note' octave out of range"));
  tone(pin, pgm_read_word(&scale[note%12])>>prescaler);
}

void nonote (int pin) {
  noTone(pin);
}
