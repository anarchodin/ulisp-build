// Interrupts

#if defined(CPU_ATmega328P)
#define NINTERRUPTS 2+1
#elif defined(CPU_ATmega2560)
#define NINTERRUPTS 8+1
#elif defined(CPU_ATmega1284P)
#define NINTERRUPTS 3+1
#endif

unsigned int InterruptCount[NINTERRUPTS];

void handleInterrupts () {
  if (tstflag(BUSY)) return;
  object *nullenv = NULL;
  setflag(BUSY);
  int ints, flag;
  cli(); flag = tstflag(INTERRUPT); clrflag(INTERRUPT); sei();
  if (flag) {
    for (int i=0; i<NINTERRUPTS; i++) {
      cli(); ints = InterruptCount[i]; InterruptCount[i] = 0; sei();
      if (ints) {
        object *pair = assoc(number(i),Events);
        object *arg = cons(number(ints), NULL);
        push(arg, GCStack);
        if (pair != NULL) apply(cdr(pair), arg, &nullenv);
        pop(GCStack);
      }
    }
  }
  clrflag(BUSY);
}

void interrupt (int n) {
  setflag(INTERRUPT);
  if (InterruptCount[n] < 0xFFFF) InterruptCount[n]++;
}

//ISR(TIMER1_OVF_vect) { interrupt(0); }
ISR(INT0_vect) { interrupt(0); }
ISR(INT1_vect) { interrupt(1); }
#if defined(CPU_ATmega1284P)
ISR(INT2_vect) { interrupt(2); }
#elif defined(CPU_ATmega2560)
ISR(INT2_vect) { interrupt(2); }
ISR(INT3_vect) { interrupt(3); }
ISR(INT4_vect) { interrupt(4); }
ISR(INT5_vect) { interrupt(5); }
ISR(INT6_vect) { interrupt(6); }
ISR(INT7_vect) { interrupt(7); }
#endif
