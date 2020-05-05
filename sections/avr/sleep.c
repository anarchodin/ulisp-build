// Sleep

#if !defined(__AVR_ATmega4809__) && !defined(ARDUINO_AVR_ATmega4809)
  // Interrupt vector for sleep watchdog
  ISR(WDT_vect) {
  WDTCSR |= 1<<WDIE;
}
#endif

void initsleep () {
  set_sleep_mode(SLEEP_MODE_PWR_DOWN);
}

void sleep (int secs) {
#if !defined(__AVR_ATmega4809__) && !defined(ARDUINO_AVR_ATmega4809)
  // Set up Watchdog timer for 1 Hz interrupt
  WDTCSR = 1<<WDCE | 1<<WDE;
  WDTCSR = 1<<WDIE | 6<<WDP0;     // 1 sec interrupt
  delay(100);  // Give serial time to settle
  // Disable ADC and timer 0
  ADCSRA = ADCSRA & ~(1<<ADEN);
#if defined(__AVR_ATmega328P__)
  PRR = PRR | 1<<PRTIM0;
#elif defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1284P__)
  PRR0 = PRR0 | 1<<PRTIM0;
#endif
  while (secs > 0) {
    sleep_enable();
    sleep_cpu();
    secs--;
  }
  WDTCSR = 1<<WDCE | 1<<WDE;     // Disable watchdog
  WDTCSR = 0;
  // Enable ADC and timer 0
  ADCSRA = ADCSRA | 1<<ADEN;
#if defined(__AVR_ATmega328P__)
  PRR = PRR & ~(1<<PRTIM0);
#elif defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1284P__)
  PRR0 = PRR0 & ~(1<<PRTIM0);
#endif
#else
  delay(1000*secs);
#endif
}
