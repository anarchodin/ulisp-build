#include "ulisp.h"

object *fn_attachinterrupt (object *args, object *env) {
  (void) env;
  object *number = first(args);
  if (number == NULL) {
    int n = NINTERRUPTS;
    args = cdr(args);
    delassoc(number,&Events);
    push(cons(number,first(args)),Events);
    InterruptCount[n] = 0;
    TCCR1A = 0;                    // CTC mode
    TCCR1B = 1<<WGM12 | 5<<CS10;   // Prescaler 1024
    OCR1A = 15624;                 // 1 sec
    TIMSK1 = 1<<TOIE1;             // OVF interrupt
  } else {
    int n = integer(number);
    if (n<0 || n>=NINTERRUPTS-1) error3(ATTACHINTERRUPT, PSTR("invalid interrupt"));
    args = cdr(args);
    delassoc(number,&Events);
    if (args == NULL || first(args) == NULL) {
      EIMSK &= ~(1<<n);
      return nil;
    }
    push(cons(number,first(args)),Events);
    InterruptCount[n] = 0;
    int mode = 3;
    args = cdr(args);
    if (args != NULL) mode = integer(first(args));
    if (mode<0 || mode>3) error3(ATTACHINTERRUPT, PSTR("invalid mode"));
    EIMSK |= 1<<n;
    n = n<<1;
    if (n <= 6) EICRA = (EICRA & ~(3<<n)) | mode<<n;
    #if NINTERRUPTS > 4
    else { n = n & 0x03; EICRB = (EICRB & ~(3<<n)) | mode<<n; }
    #endif
  }
  return nil;
}
