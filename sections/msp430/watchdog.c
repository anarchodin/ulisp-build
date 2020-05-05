// Watchdog
/* -- was disabled in prior code
void watchdogenable (int interval) {
  WDTCTL = 0x5a<<8 | ((interval == 0) ? 2 : 1);
}

void watchdogreset () {
  WDTCTL = 0x5a<<8 | 1<<WDTCNTCL | 1 ;
}
*/
