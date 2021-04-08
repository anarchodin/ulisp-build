//;; (pinmode :min 2 :max 2)
object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(PINMODE, first(args));
  PinMode pm = INPUT;
  object *arg = second(args);
  if (keywordp(arg)) pm = (PinMode)checkkeyword(PINMODE, arg);
  else if (intp(arg)) {
    int mode = getint(arg);
    if (mode == 1) pm = OUTPUT; else if (mode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (mode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (arg != nil) pm = OUTPUT;
  pinMode(pin, pm);
  return nil;
}

//;; (digitalread :min 1 :max 1)
object *fn_digitalread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALREAD, first(args));
  if (digitalRead(pin) != 0) return tee; else return nil;
}

//;; (digitalwrite :min 2 :max 2)
object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALWRITE, first(args));
  object *arg = second(args);
  int mode;
  if (keywordp(arg)) mode = checkkeyword(DIGITALWRITE, arg);
  else if (intp(arg)) mode = getint(arg) ? HIGH : LOW;
  else mode = (arg != nil) ? HIGH : LOW;
  digitalWrite(pin, mode);
  return arg;
}

//;; (analogread :min 1 :max 1)
object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin;
  object *arg = first(args);
  if (keywordp(arg)) pin = checkkeyword(ANALOGREAD, arg);
  else {
    pin = checkinteger(ANALOGREAD, arg);
    checkanalogread(pin);
  }
  return number(analogRead(pin));
}

//;; (analogwrite :min 2 :max 2)
object *fn_analogwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGWRITE, first(args));
  checkanalogwrite(pin);
  object *value = second(args);
  analogWrite(pin, checkinteger(ANALOGWRITE, value));
  return value;
}

//;; (delay :min 1 :max 1)
object *fn_delay (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  delay(checkinteger(DELAY, arg1));
  return arg1;
}

//;; (millis :max 0)
object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}

//;; (for-millis :type :special)
object *sp_formillis (object *args, object *env) {
  if (args == NULL) error2(FORMILLIS, noargument);
  object *param = first(args);
  unsigned long start = millis();
  unsigned long now, total = 0;
  if (param != NULL) total = checkinteger(FORMILLIS, eval(first(param), env));
  eval(tf_progn(cdr(args),env), env);
  do {
    now = millis() - start;
    testescape();
  } while (now < total);
  if (now <= INT_MAX) return number(now);
  return nil;
}

//;; (sleep :min 1 :max 1)
object *fn_sleep (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  sleep(checkinteger(SLEEP, arg1));
  return arg1;
}

//;; (note :min 0 :max 3)
object *fn_note (object *args, object *env) {
  (void) env;
  static int pin = 255;
  if (args != NULL) {
    pin = checkinteger(NOTE, first(args));
    int note = 0;
    if (cddr(args) != NULL) note = checkinteger(NOTE, second(args));
    int octave = 0;
    if (cddr(args) != NULL) octave = checkinteger(NOTE, third(args));
    playnote(pin, note, octave);
  } else nonote(pin);
  return nil;
}
