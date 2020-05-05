// Watchdog

void watchdogenable (int interval) {
  int i = 5;
  while (interval) { interval = interval>>1; i++; }
  wdt_enable(i);
}

void watchdogreset () {
  wdt_reset();
}
