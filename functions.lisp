;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Function definitions

(defparameter *definitions*

  '(("Symbols"
     ((NIL "nil" 0 0 nil)
      (TEE "t" 0 0 nil)
      (NOTHING nil 0 0 nil)
      (OPTIONAL "&optional" 0 0 nil)

      #-avr
      (INITIALELEMENT ":initial-element" 0 0 nil)

      (AMPREST "&rest" 0 0 nil)
      (LAMBDA nil 0 127 nil)
      (LET "let" 0 127 nil)
      (LETSTAR "let*" 0 127 nil)
      (CLOSURE nil 0 127 nil)))

    ("Special forms"
     ((SPECIAL_FORMS "" 0 0 nil)
      (QUOTE nil 1 1 "
object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, args); 
  return first(args);
}")

      #+ignore
      (LAMBDA nil 0 127 "
object *sp_lambda (object *args, object *env) {
  return cons(symbol(CLOSURE), (cons(env,args)));
}")

      (DEFUN nil 2 127 #"
object *sp_defun (object *args, object *env) {
  (void) env;
  checkargs(DEFUN, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFUN, notasymbol, var);
  object *val = cons(symbol(LAMBDA), cdr(args));
  object *pair = value(var->name,GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}"#)

      (DEFVAR nil 1 2 #"
object *sp_defvar (object *args, object *env) {
  checkargs(DEFVAR, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFVAR, notasymbol, var);
  object *val = NULL;
  args = cdr(args);
  if (args != NULL) { setflag(NOESC); val = eval(first(args), env); clrflag(NOESC); }
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}"#)

     (SETQ nil 2 126 #"
object *sp_setq (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETQ, oddargs);
    object *pair = findvalue(first(args), env);
    arg = eval(second(args), env);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}"#)

     #-esp
     (LOOP nil 0 127 "
object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
  }
}")

     #+esp
     (LOOP nil 0 127 "
object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    yield();
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
  }
}")

      (RETURN nil 0 127 "
object *sp_return (object *args, object *env) {
  object *result = eval(tf_progn(args,env), env);
  setflag(RETURNFLAG);
  return result;
}")

     (PUSH nil 2 2 "
object *sp_push (object *args, object *env) {
  checkargs(PUSH, args); 
  object *item = eval(first(args), env);
  object **loc = place(PUSH, second(args), env);
  push(item, *loc);
  return *loc;
}")

     (POP nil 1 1 "
object *sp_pop (object *args, object *env) {
  checkargs(POP, args); 
  object **loc = place(POP, first(args), env);
  object *result = car(*loc);
  pop(*loc);
  return result;
}")) "sp")


     ("Accessors"
      (
       #-float
       (INCF nil 1 2 #"
/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_incf (object *args, object *env) {
  checkargs(INCF, args); 
  object **loc = place(INCF, first(args), env);
  int increment = 1;
  int result = checkinteger(INCF, *loc);
  args = cdr(args);
  if (args != NULL) increment = checkinteger(INCF, eval(first(args), env));
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(INCF, overflow); }
  else { if (INT_MAX - increment < result) error2(INCF, overflow); }
  #endif
  result = result + increment;
  *loc = number(result);
  return *loc;
}"#)

       #+float
       (INCF nil 1 2 #"
/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_incf (object *args, object *env) {
  checkargs(INCF, args); 
  object **loc = place(INCF, first(args), env);
  args = cdr(args);
  
  object *x = *loc;
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (floatp(x) || floatp(inc)) {
    float increment;
    float value = checkintfloat(INCF, x);

    if (inc == NULL) increment = 1.0;
    else increment = checkintfloat(INCF, inc);

    *loc = makefloat(value + increment);
  } else if (integerp(x) && (integerp(inc) || inc == NULL)) {
    int increment;
    int value = x->integer;

    if (inc == NULL) increment = 1;
    else increment = inc->integer;

    if (increment < 1) {
      if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    } else {
      if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    }
  } else error2(INCF, notanumber);
  return *loc;
}"#)

    #-float
    (DECF nil 1 2 #"
/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_decf (object *args, object *env) {
  checkargs(DECF, args); 
  object **loc = place(DECF, first(args), env);
  int decrement = 1;
  int result = checkinteger(DECF, *loc);
  args = cdr(args);
  if (args != NULL) decrement = checkinteger(DECF, eval(first(args), env));
  #if defined(checkoverflow)
  if (decrement < 1) { if (INT_MAX + decrement < result) error2(DECF, overflow); }
  else { if (INT_MIN + decrement > result) error2(DECF, overflow); }
  #endif
  result = result - decrement;
  *loc = number(result);
  return *loc;
}"#)

    #+float
    (DECF nil 1 2 #"
/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls place() to get a pointer to the numeric value.
*/
object *sp_decf (object *args, object *env) {
  checkargs(DECF, args); 
  object **loc = place(DECF, first(args), env);
  args = cdr(args);
  
  object *x = *loc;
  object *dec = (args != NULL) ? eval(first(args), env) : NULL;

  if (floatp(x) || floatp(dec)) {
    float decrement;
    float value = checkintfloat(DECF, x);

    if (dec == NULL) decrement = 1.0;
    else decrement = checkintfloat(DECF, dec);

    *loc = makefloat(value - decrement);
  } if (integerp(x) && (integerp(dec) || dec == NULL)) {
    int decrement;
    int value = x->integer;

    if (dec == NULL) decrement = 1;
    else decrement = dec->integer;

    if (decrement < 1) {
      if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    } else {
      if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    }
  } else error2(DECF, notanumber);
  return *loc;
}"#)

     (SETF nil 2 126 #"
object *sp_setf (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, oddargs);
    object **loc = place(SETF, first(args), env);
    arg = eval(second(args), env);
    *loc = arg;
    args = cddr(args);
  }
  return arg;
}"#)) "sp")

     ("Other special forms"
      (
       (DOLIST nil 1 127 #"
object *sp_dolist (object *args, object *env) {
  if (args == NULL) error2(DOLIST, noargument);
  object *params = first(args);
  object *var = first(params);
  object *list = eval(second(params), env);
  push(list, GCStack); // Don't GC the list
  object *pair = cons(var,nil);
  push(pair,env);
  params = cdr(cdr(params));
  args = cdr(args);
  while (list != NULL) {
    if (improperp(list)) error(DOLIST, notproper, list);
    cdr(pair) = first(list);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        pop(GCStack);
        return result;
      }
      forms = cdr(forms);
    }
    list = cdr(list);
  }
  cdr(pair) = nil;
  pop(GCStack);
  if (params == NULL) return nil;
  return eval(car(params), env);
}"#)

      (DOTIMES nil 1 127 "
object *sp_dotimes (object *args, object *env) {
  if (args == NULL) error2(DOTIMES, noargument);
  object *params = first(args);
  object *var = first(params);
  int count = checkinteger(DOTIMES, eval(second(params), env));
  int index = 0;
  params = cdr(cdr(params));
  object *pair = cons(var,number(0));
  push(pair,env);
  args = cdr(args);
  while (index < count) {
    cdr(pair) = number(index);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      forms = cdr(forms);
    }
    index++;
  }
  cdr(pair) = number(index);
  if (params == NULL) return nil;
  return eval(car(params), env);
}")

      (TRACE nil 0 1 #"
object *sp_trace (object *args, object *env) {
  (void) env;
  while (args != NULL) {
      trace(first(args)->name);
      args = cdr(args);
  }
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
    i++;
  }
  return args;
}"#)

      (UNTRACE nil 0 1 "
object *sp_untrace (object *args, object *env) {
  (void) env;
  if (args == NULL) {
    int i = 0;
    while (i < TRACEMAX) {
      if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
      TraceFn[i] = 0;
      i++;
    }
  } else {
    while (args != NULL) {
      untrace(first(args)->name);
      args = cdr(args);
    }
  }
  return args;
}")

      (FORMILLIS "for-millis" 1 127 "
object *sp_formillis (object *args, object *env) {
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
}")

      #-avr
      (WITHOUTPUTTOSTRING "with-output-to-string" 1 127 "
object *sp_withoutputtostring (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHOUTPUTTOSTRING, nostream);
  object *var = first(params);
  object *pair = cons(var, stream(STRINGSTREAM, 0));
  push(pair,env);
  object *string = startstring(WITHOUTPUTTOSTRING);
  object *forms = cdr(args);
  eval(tf_progn(forms,env), env);
  string->cdr = GlobalString;
  GlobalString = NULL;
  return string;
}")

      (WITHSERIAL "with-serial" 1 127 "
object *sp_withserial (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSERIAL, nostream);
  object *var = first(params);
  int address = checkinteger(WITHSERIAL, eval(second(params), env));
  params = cddr(params);
  int baud = 96;
  if (params != NULL) baud = checkinteger(WITHSERIAL, eval(first(params), env));
  object *pair = cons(var, stream(SERIALSTREAM, address));
  push(pair,env);
  serialbegin(address, baud);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  serialend(address);
  return result;
}")

      (WITHI2C "with-i2c" 1 127 "
object *sp_withi2c (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHI2C, nostream);
  object *var = first(params);
  int address = checkinteger(WITHI2C, eval(second(params), env));
  params = cddr(params);
  int read = 0; // Write
  I2CCount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2CCount = rw->integer;
    read = (rw != NULL);
  }
  I2Cinit(1); // Pullups
  object *pair = cons(var, (I2Cstart(address, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(read);
  return result;
}")

      #+avr
      (WITHSPI "with-spi" 1 127 #"
object *sp_withspi (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSPI, nostream);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(WITHSPI, nostream);
  int pin = checkinteger(WITHSPI, eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(WITHSPI, eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(WITHSPI, eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(WITHSPI, eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin));
  push(pair,env);
  SPI.begin();
  SPI.beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  SPI.endTransaction();
  return result;
}"#)

      #+arm
      (WITHSPI "with-spi" 1 127 #"
object *sp_withspi (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSPI, nostream);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(WITHSPI, nostream);
  int pin = checkinteger(WITHSPI, eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0, address = 0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(WITHSPI, eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(WITHSPI, eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(WITHSPI, eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
        params = cdr(params);
        if (params != NULL) {
          address = checkinteger(WITHSPI, eval(car(params), env));
        }
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin + 128*address));
  push(pair,env);
  SPIClass *spiClass = &SPI;
  #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4) || defined(ARDUINO_PYBADGE_M4) || defined(ARDUINO_PYGAMER_M4)
  if (address == 1) spiClass = &SPI1;
  #endif
  (*spiClass).begin();
  (*spiClass).beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  (*spiClass).endTransaction();
  return result;
}"#)

      #+riscv
      (WITHSPI "with-spi" 1 127 #"
object *sp_withspi (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSPI, nostream);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(WITHSPI, nostream);
  int pin = checkinteger(WITHSPI, eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0, address = 0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(WITHSPI, eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(WITHSPI, eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(WITHSPI, eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
        params = cdr(params);
        if (params != NULL) {
          address = checkinteger(WITHSPI, eval(car(params), env));
        }
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin + 128*address));
  push(pair,env);
  SPIClass *spiClass = &SPI;
  #if defined(ARDUINO_NRF52840_CLUE) || defined(ARDUINO_GRAND_CENTRAL_M4)
  if (address == 1) spiClass = &SPI1;
  #endif
  (*spiClass).begin();
  (*spiClass).beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  (*spiClass).endTransaction();
  return result;
}"#)

      #-esp
      (WITHSDCARD "with-sd-card" 2 127 #"
object *sp_withsdcard (object *args, object *env) {
#if defined(sdcardsupport)
  object *params = first(args);
  if (params == NULL) error2(WITHSDCARD, nostream);
  object *var = first(params);
  object *filename = eval(second(params), env);
  params = cddr(params);
  SD.begin(SDCARD_SS_PIN);
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(WITHSDCARD, first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    SDpfile = SD.open(MakeFilename(filename), oflag);
    if (!SDpfile) error2(WITHSDCARD, PSTR("problem writing to SD card"));
  } else {
    SDgfile = SD.open(MakeFilename(filename), oflag);
    if (!SDgfile) error2(WITHSDCARD, PSTR("problem reading from SD card"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
#else
  (void) args, (void) env;
  error2(WITHSDCARD, PSTR("not supported"));
  return nil;
#endif
}"#)

      #+esp
      (WITHSDCARD "with-sd-card" 2 127 #"
object *sp_withsdcard (object *args, object *env) {
#if defined(sdcardsupport)
  object *params = first(args);
  object *var = first(params);
  object *filename = eval(second(params), env);
  params = cddr(params);
  SD.begin();
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(WITHSDCARD, first(params));
  const char *oflag = FILE_READ;
  if (mode == 1) oflag = FILE_APPEND; else if (mode == 2) oflag = FILE_WRITE;
  if (mode >= 1) {
    SDpfile = SD.open(MakeFilename(filename), oflag);
    if (!SDpfile) error2(WITHSDCARD, PSTR("problem writing to SD card"));
  } else {
    SDgfile = SD.open(MakeFilename(filename), oflag);
    if (!SDgfile) error2(WITHSDCARD, PSTR("problem reading from SD card"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
#else
  (void) args, (void) env;
  error2(WITHSDCARD, PSTR("not supported"));
  return nil;
#endif
}"#)

    #+gfx
    (WITHGFX "with-gfx" 1 127 #"
object *sp_withgfx (object *args, object *env) {
#if defined(gfxsupport)
  object *params = first(args);
  object *var = first(params);
  object *pair = cons(var, stream(GFXSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
#else
  (void) args, (void) env;
  error2(WITHGFX, PSTR("not supported"));
  return nil;
#endif
}"#)

    #+msp430
    (WITHLCD "with-lcd" 1 127 #"
object *sp_withlcd (object *args, object *env) {
#if defined(__MSP430FR6989__)
  myLCD.init();
  object *params = first(args);
  object *var = first(params);
  object *pair = cons(var, stream(LCDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  return result;
#else
  (void) args, (void) env;
  error(PSTR("with-lcd not supported"));
  return nil;
#endif
}"#)

    #+ethernet
    (WITHCLIENT "with-client" 1 2 #"
object *sp_withclient (object *args, object *env) {
  object *params = first(args);
  object *var = first(params);
  params = cdr(params);
  int n;
  if (params == NULL) {
    client = server.available();
    if (!client) return nil;
    n = 2;
  } else {
    object *address = eval(first(params), env);
    object *port = eval(second(params), env);
    int success;
    if (stringp(address)) success = client.connect(cstringbuf(address), checkinteger(WITHCLIENT, port));
    else if (integerp(address)) success = client.connect(address->integer, checkinteger(WITHCLIENT, port));
    else error2(WITHCLIENT, PSTR("invalid address"));
    if (!success) return nil;
    n = 1;
  }
  object *pair = cons(var, stream(WIFISTREAM, n));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  client.stop();
  return result;
}"#)) "sp")

    #+(or arm esp stm32 riscv)
    ("Assembler"
     
     (
      #+arm
      (DEFCODE nil 0 127 #"
object *sp_defcode (object *args, object *env) {
  setflag(NOESC);
  checkargs(DEFCODE, args);
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(DEFCODE, PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers r0, r1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(DEFCODE, PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), newsymbol((18*40+30+regn)*2560000)); // Symbol for r0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }
  
  // Make *pc* a local variable
  object *pcpair = cons(newsymbol(pack40((char*)"*pc*\0\0")), number(0));
  push(pcpair,env);
  args = cdr(args);
  
  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(DEFCODE, PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }        
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }
    
  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
}"#)

      #+riscv
      (DEFCODE nil 0 127 #"
object *sp_defcode (object *args, object *env) {
  setflag(NOESC);
  checkargs(DEFCODE, args);
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(DEFCODE, PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers a0, a1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(DEFCODE, PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), newsymbol((1*40+30+regn)*2560000)); // Symbol for a0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }
  
  // Make *pc* a local variable
  object *pcpair = cons(newsymbol(pack40((char*)"*pc*\0\0")), number(0));
  push(pcpair,env);
  args = cdr(args);
  
  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(DEFCODE, PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }        
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }
    
  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
}"#)) "sp")

("Tail-recursive forms"
     ((TAIL_FORMS "" 0 0 nil)
      (PROGN nil 0 127 "
object *tf_progn (object *args, object *env) {
  if (args == NULL) return nil;
  object *more = cdr(args);
  while (more != NULL) {
    object *result = eval(car(args),env);
    if (tstflag(RETURNFLAG)) return result;
    args = more;
    more = cdr(args);
  }
  return car(args);
}")

      (IF nil 2 3 #"
object *tf_if (object *args, object *env) {
  if (args == NULL || cdr(args) == NULL) error2(IF, PSTR("missing argument(s)"));
  if (eval(first(args), env) != nil) return second(args);
  args = cddr(args);
  return (args != NULL) ? first(args) : nil;
}"#)

      (COND nil 0 127 #"
object *tf_cond (object *args, object *env) {
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(COND, PSTR("illegal clause"), clause);
    object *test = eval(first(clause), env);
    object *forms = cdr(clause);
    if (test != nil) {
      if (forms == NULL) return quote(test); else return tf_progn(forms, env);
    }
    args = cdr(args);
  }
  return nil;
}"#)
      
      (WHEN nil 1 127 #"
object *tf_when (object *args, object *env) {
  if (args == NULL) error2(WHEN, noargument);
  if (eval(first(args), env) != nil) return tf_progn(cdr(args),env);
  else return nil;
}"#)

      (UNLESS nil 1 127 #"
object *tf_unless (object *args, object *env) {
  if (args == NULL) error2(UNLESS, noargument);
  if (eval(first(args), env) != nil) return nil;
  else return tf_progn(cdr(args),env);
}"#)

      (CASE nil 1 127 #"
object *tf_case (object *args, object *env) {
  object *test = eval(first(args), env);
  args = cdr(args);
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(CASE, PSTR("illegal clause"), clause);
    object *key = car(clause);
    object *forms = cdr(clause);
    if (consp(key)) {
      while (key != NULL) {
        if (eq(test,car(key))) return tf_progn(forms, env);
        key = cdr(key);
      }
    } else if (eq(test,key) || eq(key,tee)) return tf_progn(forms, env);
    args = cdr(args);
  }
  return nil;
}"#)

      (AND nil 0 127 "
object *tf_and (object *args, object *env) {
  if (args == NULL) return tee;
  object *more = cdr(args);
  while (more != NULL) {
    if (eval(car(args), env) == NULL) return nil;
    args = more;
    more = cdr(args);
  }
  return car(args);
}")

      (OR nil 0 127 "
object *tf_or (object *args, object *env) {
  while (args != NULL) {
    if (eval(car(args), env) != NULL) return car(args);
    args = cdr(args);
  }
  return nil;
}")) "tf")

    ("Core functions"
     ((FUNCTIONS "" 0 0 nil)
      (NOT nil 1 1 "
object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}")

      (NULLFN "null" 1 1 (not))

      (CONS nil 2 2 "
object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}")

      (ATOM nil 1 1 "
object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}")

      (LISTP nil 1 1 "
object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}")

      (CONSP nil 1 1 "
object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}")

      (SYMBOLP nil 1 1 #"
object *fn_symbolp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return symbolp(arg) ? tee : nil;
}"#)

      #-avr
      (ARRAYP nil 1 1 #"
object *fn_arrayp (object *args, object *env) {
  (void) env;
  return arrayp(first(args)) ? tee : nil;
}"#)

      #-avr
      (BOUNDP nil 1 1 #"
object *fn_boundp (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(BOUNDP, notasymbol, var);
  return boundp(var, env) ? tee : nil;
}"#)

      #-avr
      (SETFN nil 2 126 #"
object *fn_setfn (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETFN, oddargs);
    object *pair = findvalue(first(args), env);
    arg = second(args);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}"#)

      (STREAMP nil 1 1 #"
object *fn_streamp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return streamp(arg) ? tee : nil;
}"#)

      (EQ nil 2 2 "
object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}")))

    ("List functions"
     ((CAR nil 1 1 cxr)
      (FIRST nil 1 1 (car))
      (CDR nil 1 1 cxr)
      (REST nil 1 1 (cdr))
      (CAAR nil 1 1 cxr)
      (CADR nil 1 1 cxr)
      (SECOND nil 1 1 (cadr))
      (CDAR nil 1 1 cxr)
      (CDDR nil 1 1 cxr)
      (CAAAR nil 1 1 cxr)
      (CAADR nil 1 1 cxr)
      (CADAR nil 1 1 cxr)
      (CADDR nil 1 1 cxr)
      (THIRD nil 1 1 (caddr))
      (CDAAR nil 1 1 cxr)
      (CDADR nil 1 1 cxr)
      (CDDAR nil 1 1 cxr)
      (CDDDR nil 1 1 cxr)

      #+avr
      (LENGTH nil 1 1 #"
object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (!stringp(arg)) error(LENGTH, PSTR("argument is not a list or string"), arg);
  return number(stringlength(arg));
}"#)
      
      #-avr
      (LENGTH nil 1 1 #"
object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (stringp(arg)) return number(stringlength(arg));
  if (arrayp(arg) && integerp(cddr(arg))) return number(cddr(arg)->integer);
  error(LENGTH, PSTR("argument is not a list, 1d array, or string"), arg);
}"#)

      #-avr
      (ARRAYDIMENSIONS "array-dimensions" 1 1 #"
object *fn_arraydimensions (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(ARRAYDIMENSIONS, PSTR("argument is not an array"), array);
  return cddr(array);
}"#)

      (LIST nil 0 127 "
object *fn_list (object *args, object *env) {
  (void) env;
  return args;
}")

      #-avr
      (MAKEARRAY "make-array" 1 3 #"
object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  int xd, yd = 1;
  object *dimensions = first(args);
  if (dimensions == NULL) error2(MAKEARRAY, PSTR("dimensions can't be nil"));
  else if (atom(dimensions)) dimensions = cons(dimensions, NULL);
 
  if (cdr(dimensions) == NULL) xd = checkinteger(MAKEARRAY, first(dimensions));
  else if (cddr(dimensions) == NULL) {
    xd = checkinteger(MAKEARRAY, first(dimensions));
    yd = checkinteger(MAKEARRAY, second(dimensions));
  } else error2(MAKEARRAY, PSTR("only two dimensions supported"));
  if (xd < 0 || yd < 0) error2(MAKEARRAY, PSTR("dimension can't be negative"));
  if (cdr(args) != NULL) {
    object *var = second(args);
    if (!symbolp(var) || var->name != INITIALELEMENT)
      error(MAKEARRAY, PSTR("illegal second argument"), var); 
    if (cddr(args) != NULL) def = third(args);
  }
  return makearray(xd, yd, dimensions, def);
}"#)

      (REVERSE nil 1 1 #"
object *fn_reverse (object *args, object *env) {
  (void) env;
  object *list = first(args);
  object *result = NULL;
  while (list != NULL) {
    if (improperp(list)) error(REVERSE, notproper, list);
    push(first(list),result);
    list = cdr(list);
  }
  return result;
}"#)

      (NTH nil 2 2 #"
object *fn_nth (object *args, object *env) {
  (void) env;
  int n = checkinteger(NTH, first(args));
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(NTH, notproper, list);
    if (n == 0) return car(list);
    list = cdr(list);
    n--;
  }
  return nil;
}"#)

      #-avr
      (AREF nil 2 3 #"
object *fn_aref (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
  object *dimensions = cddr(array);
  int y = 0, x = checkinteger(AREF, second(args));
  if (listp(dimensions)) {
    if (cddr(args) == NULL) error2(AREF, PSTR("array needs two subscripts"));
    y = checkinteger(AREF, third(args));
  } else if (cddr(args) != NULL) error2(AREF, PSTR("array needs one subscript"));
  return *getarray(AREF, array, x, y);
}"#)

      (ASSOC nil 2 2 #"
object *fn_assoc (object *args, object *env) {
  (void) env;
  object *key = first(args);
  object *list = second(args);
  return assoc(key,list);
}"#)

      (MEMBER nil 2 2 #"
object *fn_member (object *args, object *env) {
  (void) env;
  object *item = first(args);
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(MEMBER, notproper, list);
    if (eq(item,car(list))) return list;
    list = cdr(list);
  }
  return nil;
}"#)

      (APPLY nil 2 127 #"
object *fn_apply (object *args, object *env) {
  object *previous = NULL;
  object *last = args;
  while (cdr(last) != NULL) {
    previous = last;
    last = cdr(last);
  }
  object *arg = car(last);
  if (!listp(arg)) error(APPLY, PSTR("last argument is not a list"), arg);
  cdr(previous) = arg;
  return apply(APPLY, first(args), cdr(args), env);
}"#)

      (FUNCALL nil 1 127 "
object *fn_funcall (object *args, object *env) {
  return apply(FUNCALL, first(args), cdr(args), env);
}")

      (APPEND nil 0 127 #"
object *fn_append (object *args, object *env) {
  (void) env;
  object *head = NULL;
  object *tail;
  while (args != NULL) {   
    object *list = first(args);
    if (!listp(list)) error(APPEND, notalist, list);
    while (consp(list)) {
      object *obj = cons(car(list), cdr(list));
      if (head == NULL) head = obj;
      else cdr(tail) = obj;
      tail = obj;
      list = cdr(list);
      if (cdr(args) != NULL && improperp(list)) error(APPEND, notproper, first(args));
    }
    args = cdr(args);
  }
  return head;
}"#)

      (MAPC nil 2 127 #"
object *fn_mapc (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *result = first(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         return result;
      }
      if (improperp(list)) error(MAPC, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    apply(MAPC, function, cdr(params), env);
  }
}"#)

      (MAPCAR nil 2 127 #"
object *fn_mapcar (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL); 
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(MAPCAR, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(MAPCAR, function, cdr(params), env);
    object *obj = cons(result,NULL);
    cdr(tail) = obj; tail = obj;
  }
}"#)

      (MAPCAN nil 2 127 #"
object *fn_mapcan (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL); 
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(MAPCAN, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(MAPCAN, function, cdr(params), env);
    while (consp(result)) {
      cdr(tail) = result; tail = result;
      result = cdr(result);
    }
    if (result != NULL) error(MAPCAN, resultproper, result);
  }
}"#)))

    ("Arithmetic functions"
     (

      #-float
      (ADD "+" 0 127 #"
object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    int temp = checkinteger(ADD, car(args));
    #if defined(checkoverflow)
    if (temp < 1) { if (INT_MIN - temp > result) error2(ADD, overflow); }
    else { if (INT_MAX - temp < result) error2(ADD, overflow); }
    #endif
    result = result + temp;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (ADD "+" 0 127 #"
object *add_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult + checkintfloat(ADD, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    object *arg = car(args);
    if (floatp(arg)) return add_floats(args, (float)result);
    else if (integerp(arg)) {
      int val = arg->integer;
      if (val < 1) { if (INT_MIN - val > result) return add_floats(args, (float)result); }
      else { if (INT_MAX - val < result) return add_floats(args, (float)result); }
      result = result + val;
    } else error(ADD, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}"#)

      #-float
      (SUBTRACT "-" 1 127 #"
object *fn_subtract (object *args, object *env) {
  (void) env;
  int result = checkinteger(SUBTRACT, car(args));
  args = cdr(args);
  if (args == NULL) {
    #if defined(checkoverflow)
    if (result == INT_MIN) error2(SUBTRACT, overflow);
    #endif
    return number(-result);
  }
  while (args != NULL) {
    int temp = checkinteger(SUBTRACT, car(args));
    #if defined(checkoverflow)
    if (temp < 1) { if (INT_MAX + temp < result) error2(SUBTRACT, overflow); }
    else { if (INT_MIN + temp > result) error2(SUBTRACT, overflow); }
    #endif
    result = result - temp;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (SUBTRACT "-" 1 127 #"
object *subtract_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult - checkintfloat(SUBTRACT, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *negate (object *arg) {
  if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(-result);
    else return number(-result);
  } else if (floatp(arg)) return makefloat(-(arg->single_float));
  else error(SUBTRACT, notanumber, arg);
}

object *fn_subtract (object *args, object *env) {
  (void) env;
  object *arg = car(args);
  args = cdr(args);
  if (args == NULL) return negate(arg);
  else if (floatp(arg)) return subtract_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) return subtract_floats(args, result);
      else if (integerp(arg)) {
        int val = (car(args))->integer;
        if (val < 1) { if (INT_MAX + val < result) return subtract_floats(args, result); }
        else { if (INT_MIN + val > result) return subtract_floats(args, result); }
        result = result - val;
      } else error(SUBTRACT, notanumber, arg);
      args = cdr(args);
    }
    return number(result);
  } else error(SUBTRACT, notanumber, arg);
}"#)

      #-float
      (MULTIPLY "*" 0 127 #"
object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    #if defined(checkoverflow)
    signed long temp = (signed long) result * checkinteger(MULTIPLY, car(args));
    if ((temp > INT_MAX) || (temp < INT_MIN)) error2(MULTIPLY, overflow);
    result = temp;
    #else
    result = result * checkinteger(MULTIPLY, car(args));
    #endif
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MULTIPLY "*" 0 127 #"
object *multiply_floats (object *args, float fresult) {
  while (args != NULL) {
   object *arg = car(args);
    fresult = fresult * checkintfloat(MULTIPLY, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    object *arg = car(args);
    if (floatp(arg)) return multiply_floats(args, result);
    else if (integerp(arg)) {
      int64_t val = result * (int64_t)(arg->integer);
      if ((val > INT_MAX) || (val < INT_MIN)) return multiply_floats(args, result);
      result = val;
    } else error(MULTIPLY, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}"#)

      #-float
      (DIVIDE "/" 2 127 #"
object *fn_divide (object *args, object *env) {
  (void) env;
  int result = checkinteger(DIVIDE, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg = checkinteger(DIVIDE, car(args));
    if (arg == 0) error2(DIVIDE, PSTR("division by zero"));
    #if defined(checkoverflow)
    if ((result == INT_MIN) && (arg == -1)) error2(DIVIDE, overflow);
    #endif
    result = result / arg;
    args = cdr(args);
  }
  return number(result);
}"#)

     #-float
     (TRUNCATE nil 1 2 (divide))

     #+float
     (DIVIDE "/" 1 127 #"
object *divide_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    float f = checkintfloat(DIVIDE, arg);
    if (f == 0.0) error2(DIVIDE, PSTR("division by zero"));
    fresult = fresult / f;
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_divide (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  args = cdr(args);
  // One argument
  if (args == NULL) {
    if (floatp(arg)) {
      float f = arg->single_float;
      if (f == 0.0) error2(DIVIDE, PSTR("division by zero"));
      return makefloat(1.0 / f);
    } else if (integerp(arg)) {
      int i = arg->integer;
      if (i == 0) error2(DIVIDE, PSTR("division by zero"));
      else if (i == 1) return number(1);
      else return makefloat(1.0 / i);
    } else error(DIVIDE, notanumber, arg);
  }    
  // Multiple arguments
  if (floatp(arg)) return divide_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) {
        return divide_floats(args, result);
      } else if (integerp(arg)) {       
        int i = arg->integer;
        if (i == 0) error2(DIVIDE, PSTR("division by zero"));
        if ((result % i) != 0) return divide_floats(args, result);
        if ((result == INT_MIN) && (i == -1)) return divide_floats(args, result);
        result = result / i;
        args = cdr(args);
      } else error(DIVIDE, notanumber, arg);
    }
    return number(result); 
  } else error(DIVIDE, notanumber, arg);
}"#)

     #-float
     (MOD nil 2 2 #"
object *fn_mod (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(MOD, first(args));
  int arg2 = checkinteger(MOD, second(args));
  if (arg2 == 0) error2(MOD, PSTR("division by zero"));
  int r = arg1 % arg2;
  if ((arg1<0) != (arg2<0)) r = r + arg2;
  return number(r);
}"#)

     #+float
     (MOD nil 2 2 #"
object *fn_mod (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  object *arg2 = second(args);
  if (integerp(arg1) && integerp(arg2)) {
    int divisor = arg2->integer;
    if (divisor == 0) error2(MOD, PSTR("division by zero"));
    int dividend = arg1->integer;
    int remainder = dividend % divisor;
    if ((dividend<0) != (divisor<0)) remainder = remainder + divisor;
    return number(remainder);
  } else {
    float fdivisor = checkintfloat(MOD, arg2);
    if (fdivisor == 0.0) error2(MOD, PSTR("division by zero"));
    float fdividend = checkintfloat(MOD, arg1);
    float fremainder = fmod(fdividend , fdivisor);
    if ((fdividend<0) != (fdivisor<0)) fremainder = fremainder + fdivisor;
    return makefloat(fremainder);
  }
}"#)

      #-float
      (ONEPLUS "1+" 1 1 #"
object *fn_oneplus (object *args, object *env) {
  (void) env;
  int result = checkinteger(ONEPLUS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MAX) error2(ONEPLUS, overflow);
  #endif
  return number(result + 1);
}"#)

      #+float
      (ONEPLUS "1+" 1 1 #"
object *fn_oneplus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) + 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MAX) return makefloat((arg->integer) + 1.0);
    else return number(result + 1);
  } else error(ONEPLUS, notanumber, arg);
}"#)

      #-float
      (ONEMINUS "1-" 1 1 #"
object *fn_oneminus (object *args, object *env) {
  (void) env;
  int result = checkinteger(ONEMINUS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(ONEMINUS, overflow);
  #endif
  return number(result - 1);
}"#)

      #+float
      (ONEMINUS "1-" 1 1 #"
object *fn_oneminus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) - 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat((arg->integer) - 1.0);
    else return number(result - 1);
  } else error(ONEMINUS, notanumber, arg);
}"#)

      #-float
      (ABS nil 1 1 #"
object *fn_abs (object *args, object *env) {
  (void) env;
  int result = checkinteger(ABS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(ABS, overflow);
  #endif
  return number(abs(result));
}"#)

      #+float
      (ABS nil 1 1 #"
object *fn_abs (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return makefloat(abs(arg->single_float));
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(abs((float)result));
    else return number(abs(result));
  } else error(ABS, notanumber, arg);
}"#)

      #-float
      (RANDOM nil 1 1 numeric1)

      #+float
      (RANDOM nil 1 1 #"
object *fn_random (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (integerp(arg)) return number(random(arg->integer));
  else if (floatp(arg)) return makefloat((float)rand()/(float)(RAND_MAX/(arg->single_float)));
  else error(RANDOM, notanumber, arg);
}"#)

      #-float
      (MAXFN "max" 1 127 #"
object *fn_maxfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(MAXFN, first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(MAXFN, car(args));
    if (next > result) result = next;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MAXFN "max" 1 127 #"
object *fn_maxfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) > (result->integer)) result = arg;
    } else if ((checkintfloat(MAXFN, arg) > checkintfloat(MAXFN, result))) result = arg;
    args = cdr(args); 
  }
  return result;
}"#)

      #-float
      (MINFN "min" 1 127 #"
object *fn_minfn (object *args, object *env) {
  (void) env;
  int result = checkinteger(MINFN, first(args));
  args = cdr(args);
  while (args != NULL) {
    int next = checkinteger(MINFN, car(args));
    if (next < result) result = next;
    args = cdr(args);
  }
  return number(result);
}"#)

      #+float
      (MINFN "min" 1 127 #"
object *fn_minfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) < (result->integer)) result = arg;
    } else if ((checkintfloat(MINFN, arg) < checkintfloat(MINFN, result))) result = arg;
    args = cdr(args); 
  }
  return result;
}"#)))

    ("Arithmetic comparisons"

     (

      #-float
      (NOTEQ "/=" 1 127 "
object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {   
    object *nargs = args;
    int arg1 = checkinteger(NOTEQ, first(nargs));
    nargs = cdr(nargs);
    while (nargs != NULL) {
       int arg2 = checkinteger(NOTEQ, first(nargs));
       if (arg1 == arg2) return nil;
       nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}")

      #+float
      (NOTEQ "/=" 1 127 #"
object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *nargs = args;
    object *arg1 = first(nargs);
    nargs = cdr(nargs);
    while (nargs != NULL) {
      object *arg2 = first(nargs);
      if (integerp(arg1) && integerp(arg2)) {
        if ((arg1->integer) == (arg2->integer)) return nil;
      } else if ((checkintfloat(NOTEQ, arg1) == checkintfloat(NOTEQ, arg2))) return nil;
      nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}"#)

      (NUMEQ "=" 1 127 comparison)
      (LESS "<" 1 127 comparison)
      (LESSEQ "<=" 1 127 comparison)
      (GREATER ">" 1 127 comparison)
      (GREATEREQ ">=" 1 127 comparison)

      #-float
      (PLUSP nil 1 1 "
object *fn_plusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(PLUSP, first(args));
  if (arg > 0) return tee;
  else return nil;
}")

      #+float
      (PLUSP nil 1 1 "
object *fn_plusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) > 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) > 0) ? tee : nil;
  else error(PLUSP, notanumber, arg);
}")

      #-float
      (MINUSP nil 1 1 "
object *fn_minusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(MINUSP, first(args));
  if (arg < 0) return tee;
  else return nil;
}")

      #+float
      (MINUSP nil 1 1 "
object *fn_minusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) < 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) < 0) ? tee : nil;
  else error(MINUSP, notanumber, arg);
}")

      #-float
      (ZEROP nil 1 1 "
object *fn_zerop (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ZEROP, first(args));
  return (arg == 0) ? tee : nil;
}")

      #+float
      (ZEROP nil 1 1 "
object *fn_zerop (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) == 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) == 0) ? tee : nil;
  else error(ZEROP, notanumber, arg);
}")

      (ODDP nil 1 1 "
object *fn_oddp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ODDP, first(args));
  return ((arg & 1) == 1) ? tee : nil;
}")

      (EVENP nil 1 1 "
object *fn_evenp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(EVENP, first(args));
  return ((arg & 1) == 0) ? tee : nil;
}")))

    ("Number functions"

     ((INTEGERP nil 1 1 #"
object *fn_integerp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}"#)

     #-float
     (NUMBERP nil 1 1 #"
object *fn_numberp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}"#)

     #+float
     (NUMBERP nil 1 1 #"
object *fn_numberp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (integerp(arg) || floatp(arg)) ? tee : nil;
}"#)))

      #+float
      ("Floating-point functions"
       ((FLOATFN "float" 1 1 #"
object *fn_floatfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (floatp(arg)) ? arg : makefloat((float)(arg->integer));
}"#)

    (FLOATP nil 1 1 #"
object *fn_floatp (object *args, object *env) {
  (void) env;
  return floatp(first(args)) ? tee : nil;
}"#)

    (SIN nil 1 1 float-function)
    (COS nil 1 1 float-function)
    (TAN nil 1 1 float-function)
    (ASIN nil 1 1 float-function)
    (ACOS nil 1 1 float-function)
    (ATAN nil 1 2 #"
object *fn_atan (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float div = 1.0;
  args = cdr(args);
  if (args != NULL) div = checkintfloat(ATAN, first(args));
  return makefloat(atan2(checkintfloat(ATAN, arg), div));
}"#)
      
    (SINH nil 1 1 float-function)
    (COSH nil 1 1 float-function)
    (TANH nil 1 1 float-function)
    (EXP nil 1 1 float-function)
    (SQRT nil 1 1 float-function)

    (LOG nil 1 2 #"
object *fn_log (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float fresult = log(checkintfloat(LOG, arg));
  args = cdr(args);
  if (args == NULL) return makefloat(fresult);
  else return makefloat(fresult / log(checkintfloat(LOG, first(args))));
}"#)
    
    (EXPT nil 2 2 #"
int intpower (int base, int exp) {
  int result = 1;
  while (exp) {
    if (exp & 1) result = result * base;
    exp = exp / 2;
    base = base * base;
  }
  return result;
}

object *fn_expt (object *args, object *env) {
  (void) env;
  object *arg1 = first(args); object *arg2 = second(args);
  float float1 = checkintfloat(EXPT, arg1);
  float value = log(abs(float1)) * checkintfloat(EXPT, arg2);
  if (integerp(arg1) && integerp(arg2) && ((arg2->integer) > 0) && (abs(value) < 21.4875)) 
    return number(intpower(arg1->integer, arg2->integer));
  if (float1 < 0) error2(EXPT, PSTR("invalid result"));
  return makefloat(exp(value));
}"#)
      
    (CEILING nil 1 2 truncate-function)
    (FLOOR nil 1 2 truncate-function)
      
    (TRUNCATE nil 1 2 #"
object *fn_truncate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number((int)(checkintfloat(TRUNCATE, arg) / checkintfloat(TRUNCATE, first(args))));
  else return number((int)(checkintfloat(TRUNCATE, arg)));
}"#)

    (ROUND nil 1 2 #"
int myround (float number) {
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}

object *fn_round (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(myround(checkintfloat(ROUND, arg) / checkintfloat(ROUND, first(args))));
  else return number(myround(checkintfloat(ROUND, arg)));
}"#)))

    ("Characters"

     ((CHAR "char" 2 2 #"
object *fn_char (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(CHAR, notastring, arg);
  char c = nthchar(arg, checkinteger(CHAR, second(args)));
  if (c == 0) error2(CHAR, PSTR("index out of range"));
  return character(c);
}"#)

      (CHARCODE "char-code" 1 1 #"
object *fn_charcode (object *args, object *env) {
  (void) env;
  return number(checkchar(CHARCODE, first(args)));
}"#)

      (CODECHAR "code-char" 1 1 #"
object *fn_codechar (object *args, object *env) {
  (void) env;
  return character(checkinteger(CODECHAR, first(args)));
}"#)

      (CHARACTERP nil 1 1 #"
object *fn_characterp (object *args, object *env) {
  (void) env;
  return characterp(first(args)) ? tee : nil;
}"#)))

      ("Strings"

     ((STRINGP nil 1 1 "
object *fn_stringp (object *args, object *env) {
  (void) env;
  return stringp(first(args)) ? tee : nil;
}")

      (STRINGEQ "string=" 2 2 #"
bool stringcompare (symbol_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = first(args); if (!stringp(arg1)) error(name, notastring, arg1);
  object *arg2 = second(args); if (!stringp(arg2)) error(name, notastring, arg2); 
  arg1 = cdr(arg1);
  arg2 = cdr(arg2);
  while ((arg1 != NULL) || (arg2 != NULL)) {
    if (arg1 == NULL) return lt;
    if (arg2 == NULL) return gt;
    if (arg1->chars < arg2->chars) return lt;
    if (arg1->chars > arg2->chars) return gt;
    arg1 = car(arg1);
    arg2 = car(arg2);
  }
  return eq;
}

object *fn_stringeq (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGEQ, args, false, false, true) ? tee : nil;
}"#)

      (STRINGLESS "string<" 2 2 #"
object *fn_stringless (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGLESS, args, true, false, false) ? tee : nil;
}"#)

      (STRINGGREATER "string>" 2 2 #"
object *fn_stringgreater (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGGREATER, args, false, true, false) ? tee : nil;
}"#)

      (SORT "sort" 2 2 #"
object *fn_sort (object *args, object *env) {
  if (first(args) == NULL) return nil;
  object *list = cons(nil,first(args));
  push(list,GCStack);
  object *predicate = second(args);
  object *compare = cons(NULL, cons(NULL, NULL));
  push(compare,GCStack);
  object *ptr = cdr(list);
  while (cdr(ptr) != NULL) {
    object *go = list;
    while (go != ptr) {
      car(compare) = car(cdr(ptr));
      car(cdr(compare)) = car(cdr(go));
      if (apply(SORT, predicate, compare, env)) break;
      go = cdr(go);
    }
    if (go != ptr) {
      object *obj = cdr(ptr);
      cdr(ptr) = cdr(obj);
      cdr(obj) = cdr(go);
      cdr(go) = obj;
    } else ptr = cdr(ptr);
  }
  pop(GCStack); pop(GCStack);
  return cdr(list);
}"#)

      (STRINGFN "string" 1 1 #"
object *fn_stringfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int type = arg->type;
  if (type == STRING) return arg;
  object *obj = myalloc();
  obj->type = STRING;
  if (type == CHARACTER) {
    object *cell = myalloc();
    cell->car = NULL;
    uint8_t shift = (sizeof(int)-1)*8;
    cell->chars = (arg->chars)<<shift;
    obj->cdr = cell;
  } else if (type == SYMBOL) {
    char *s = symbolname(arg->name);
    char ch = *s++;
    object *head = NULL;
    int chars = 0;
    while (ch) {
      if (ch == '\\') ch = *s++;
      buildstring(ch, &chars, &head);
      ch = *s++;
    }
    obj->cdr = head;
  } else error(STRINGFN, PSTR("can't convert to string"), arg);
  return obj;
}"#)

    (CONCATENATE nil 1 127 #"
object *fn_concatenate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  symbol_t name = arg->name;
  if (name != STRINGFN) error2(CONCATENATE, PSTR("only supports strings"));
  args = cdr(args);
  object *result = myalloc();
  result->type = STRING;
  object *head = NULL;
  int chars = 0;
  while (args != NULL) {
    object *obj = first(args);
    if (!stringp(obj)) error(CONCATENATE, notastring, obj);
    obj = cdr(obj);
    while (obj != NULL) {
      int quad = obj->chars;
      while (quad != 0) {
         char ch = quad>>((sizeof(int)-1)*8) & 0xFF;
         buildstring(ch, &chars, &head);
         quad = quad<<8;
      }
      obj = car(obj);
    }
    args = cdr(args);
  }
  result->cdr = head;
  return result;
}"#)

    (SUBSEQ nil 2 3 #"
object *fn_subseq (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(SUBSEQ, notastring, arg);
  int start = checkinteger(SUBSEQ, second(args));
  int end;
  args = cddr(args);
  if (args != NULL) end = checkinteger(SUBSEQ, car(args)); else end = stringlength(arg);
  object *result = myalloc();
  result->type = STRING;
  object *head = NULL;
  int chars = 0;
  for (int i=start; i<end; i++) {
    char ch = nthchar(arg, i);
    if (ch == 0) error2(SUBSEQ, PSTR("index out of range"));
    buildstring(ch, &chars, &head);
  }
  result->cdr = head;
  return result;
}"#)

    (READFROMSTRING "read-from-string" 1 1 #"
object *fn_readfromstring (object *args, object *env) {   
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(READFROMSTRING, notastring, arg);
  GlobalString = arg;
  GlobalStringIndex = 0;
  return read(gstr);
}"#)

    (PRINCTOSTRING "princ-to-string" 1 1 #"
object *fn_princtostring (object *args, object *env) {   
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRINCTOSTRING);
  prin1object(arg, pstr);
  obj->cdr = GlobalString;
  return obj;
}"#)

    (PRIN1TOSTRING "prin1-to-string" 1 1 #"
object *fn_prin1tostring (object *args, object *env) {   
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRIN1TOSTRING);
  printobject(arg, pstr);
  obj->cdr = GlobalString;
  return obj;
}"#)))

    ("Bitwise operators"

     ((LOGAND nil 0 127 bitwise)
      (LOGIOR nil 0 127 bitwise)
      (LOGXOR nil 0 127 bitwise)
      
      (LOGNOT nil 1 1 "
object *fn_lognot (object *args, object *env) {
  (void) env;
  int result = checkinteger(LOGNOT, car(args));
  return number(~result);
}")

      (ASH nil 2 2 "
object *fn_ash (object *args, object *env) {
  (void) env;
  int value = checkinteger(ASH, first(args));
  int count = checkinteger(ASH, second(args));
  if (count >= 0) return number(value << count);
  else return number(value >> abs(count));
}")

      (LOGBITP nil 2 2 "
object *fn_logbitp (object *args, object *env) {
  (void) env;
  int index = checkinteger(LOGBITP, first(args));
  int value = checkinteger(LOGBITP, second(args));
  return (bitRead(value, index) == 1) ? tee : nil;
}")))

    ("System functions"
     ((EVAL nil 1 1 "
object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}")

      (GLOBALS nil 0 0 "
object *fn_globals (object *args, object *env) {
  (void) args;
  if (GlobalEnv == NULL) return nil;
  return fn_mapcar(cons(symbol(CAR),cons(GlobalEnv,nil)), env);
}")

      (LOCALS nil 0 0 "
object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}")

      (MAKUNBOUND nil 1 1 #"
object *fn_makunbound (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(MAKUNBOUND, notasymbol, var);
  delassoc(var, &GlobalEnv);
  return var;
}"#)

      (BREAK nil 0 0 #"
object *fn_break (object *args, object *env) {
  (void) args;
  pfstring(PSTR("\rBreak!\r"), pserial);
  BreakLevel++;
  repl(env);
  BreakLevel--;
  return nil;
}"#)

      (READ nil 0 1 "
object *fn_read (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return read(gfun);
}")

      (PRIN1 nil 1 2 "
object *fn_prin1 (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  printobject(obj, pfun);
  return obj;
}")

      (PRINT nil 1 2 "
object *fn_print (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  printobject(obj, pfun);
  pfun(' ');
  return obj;
}")

      (PRINC nil 1 2 "
object *fn_princ (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  prin1object(obj, pfun);
  return obj;
}")

      (TERPRI nil 0 1 "
object *fn_terpri (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  pln(pfun);
  return nil;
}")

    (READBYTE "read-byte" 0 2 #"
object *fn_readbyte (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  int c = gfun();
  return (c == -1) ? nil : number(c);
}"#)

    (READLINE "read-line" 0 1 #"
object *fn_readline (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return readstring('\n', gfun);
}"#)

    (WRITEBYTE "write-byte" 1 2 #"
object *fn_writebyte (object *args, object *env) {
  (void) env;
  int value = checkinteger(WRITEBYTE, first(args));
  pfun_t pfun = pstreamfun(cdr(args));
  (pfun)(value);
  return nil;
}"#)

    (WRITESTRING "write-string" 1 2 #"
object *fn_writestring (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  Flags = temp;
  return nil;
}"#)

    (WRITELINE "write-line" 1 2 #"
object *fn_writeline (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  pln(pfun);
  Flags = temp;
  return nil;
}"#)

      (RESTARTI2C "restart-i2c" 1 2 #"
object *fn_restarti2c (object *args, object *env) {
  (void) env;
  int stream = first(args)->integer;
  args = cdr(args);
  int read = 0; // Write
  I2CCount = 0;
  if (args != NULL) {
    object *rw = first(args);
    if (integerp(rw)) I2CCount = rw->integer;
    read = (rw != NULL);
  }
  int address = stream & 0xFF;
  if (stream>>8 != I2CSTREAM) error2(RESTARTI2C, PSTR("not an i2c stream"));
  return I2Crestart(address, read) ? tee : nil;
}"#)

      (GC nil 0 0 #"
object *fn_gc (object *obj, object *env) {
  int initial = Freespace;
  unsigned long start = micros();
  gc(obj, env);
  unsigned long elapsed = micros() - start;
  pfstring(PSTR("Space: "), pserial);
  pint(Freespace - initial, pserial);
  pfstring(PSTR(" bytes, Time: "), pserial);
  pint(elapsed, pserial);
  pfstring(PSTR(" us\r"), pserial);
  return nil;
}"#)

      (ROOM nil 0 0 #"
object *fn_room (object *args, object *env) {
  (void) args, (void) env;
  return number(Freespace);
}"#)

      (SAVEIMAGE "save-image" 0 1 "
object *fn_saveimage (object *args, object *env) {
  if (args != NULL) args = eval(first(args), env);
  return number(saveimage(args));
}")

      (LOADIMAGE "load-image" 0 1 "
object *fn_loadimage (object *args, object *env) {
  (void) env;
  if (args != NULL) args = first(args);
  return number(loadimage(args));
}")

      #+ignore
      (DUMPIMAGE "dump-image" 0 0 #"
object *fn_dumpimage(object *args, object *env) {
  (void) args, (void) env;
  int imagesize = workspacesize; // compactimage(NULL);
  char tmp[16];
  Serial.println(); 
  sprintf(tmp, "freelist: %04x, ", (int)freelist);
  Serial.print(tmp);
  sprintf(tmp, "GlobalEnv: %04x, ", (int)GlobalEnv);
  Serial.print(tmp);
  sprintf(tmp, "GCStack: %04x, ", (int)GCStack);
  Serial.print(tmp);
      
  for (int i=0; i<imagesize; i++) {
    if (i%16 == 0) {
      Serial.println(); 
      sprintf(tmp, "%04x: ", (int)&workspace[i]);
      Serial.print(tmp);
    }
    sprintf(tmp, "%04x.%04x ", (unsigned int)car(&workspace[i]) , (unsigned int)cdr(&workspace[i]));
    Serial.print(tmp);
  }
  Serial.println();
  return nil;
}"#)

      (CLS "cls" 0 0 "
object *fn_cls (object *args, object *env) {
  (void) args, (void) env;
  pserial(12);
  return nil;
}")))

    ("Arduino procedures"
     (

      #+ignore
      (WATCHDOG nil 0 1 "
object *fn_watchdog (object *args, object *env) {
  (void) env;
  if (args == NULL) watchdogreset();
  else watchdogenable(integer(first(args)));
  return nil;
}")

      #-stm32
      (PINMODE nil 2 2 "
object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(PINMODE, first(args));
  PinMode pm = INPUT;
  object *mode = second(args);
  if (integerp(mode)) {
    int nmode = mode->integer;
    if (nmode == 1) pm = OUTPUT; else if (nmode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (nmode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (mode != nil) pm = OUTPUT;
  pinMode(pin, pm);
  return nil;
}")

      #+stm32
      (PINMODE nil 2 2 "
object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(PINMODE, first(args));
  int pm = INPUT;
  object *mode = second(args);
  if (integerp(mode)) {
    int nmode = checkinteger(PINMODE, mode);
    if (nmode == 1) pm = OUTPUT; else if (nmode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (nmode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (mode != nil) pm = OUTPUT;
  pinMode(pin, (WiringPinMode)pm);
  return nil;
}")

      (DIGITALREAD nil 1 1 "
object *fn_digitalread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALREAD, first(args));
  if (digitalRead(pin) != 0) return tee; else return nil;
}")

      (DIGITALWRITE nil 2 2 "
object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALWRITE, first(args));
  object *mode = second(args);
  if (integerp(mode)) digitalWrite(pin, mode->integer ? HIGH : LOW);
  else digitalWrite(pin, (mode != nil) ? HIGH : LOW);
  return mode;
}")

      (ANALOGREAD nil 1 1 #"
object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGREAD, first(args));
  checkanalogread(pin);
  return number(analogRead(pin));
}"#)

      (ANALOGWRITE nil 2 2 #" 
object *fn_analogwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGWRITE, first(args));
  checkanalogwrite(pin);
  object *value = second(args);
  analogWrite(pin, checkinteger(ANALOGWRITE, value));
  return value;
}"#)

      (DELAY nil 1 1 "
object *fn_delay (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  delay(checkinteger(DELAY, arg1));
  return arg1;
}")

      (MILLIS nil 0 0 #"
object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}"#)

      (SLEEP nil 1 1 #"
object *fn_sleep (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  sleep(checkinteger(SLEEP, arg1));
  return arg1;
}"#)

      #+ignore
      (SHIFTOUT nil 4 4 "
object *fn_shiftout (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  object *value = fourth(args);
  shiftOut(datapin, clockpin, order, integer(value));
  return value;
}")

      #+ignore
      (SHIFTIN nil 3 3 "
object *fn_shiftin (object *args, object *env) {
  (void) env;
  int datapin = integer(first(args));
  int clockpin = integer(second(args));
  int order = (third(args) != nil);
  int value = shiftIn(datapin, clockpin, order);
  return number(value);
}")

      (NOTE nil 0 3 #"
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
}"#)

      #+interrupts
      (ATTACHINTERRUPT "attach-interrupt" 1 3 #"
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
}"#)))


    ("Tree Editor"

     ((EDIT nil 1 1 #"
object *fn_edit (object *args, object *env) {
  object *fun = first(args);
  object *pair = findvalue(fun, env);
  clrflag(EXITEDITOR);
  object *arg = edit(eval(fun, env));
  cdr(pair) = arg;
  return arg;
}

object *edit (object *fun) {
  while (1) {
    if (tstflag(EXITEDITOR)) return fun;
    char c = gserial();
    if (c == 'q') setflag(EXITEDITOR);
    else if (c == 'b') return fun;
    else if (c == 'r') fun = read(gserial);
    else if (c == '\n') { pfl(pserial); superprint(fun, 0, pserial); pln(pserial); }
    else if (c == 'c') fun = cons(read(gserial), fun);
    else if (atom(fun)) pserial('!');
    else if (c == 'd') fun = cons(car(fun), edit(cdr(fun)));
    else if (c == 'a') fun = cons(edit(car(fun)), cdr(fun));
    else if (c == 'x') fun = cdr(fun);
    else pserial('?');
  }
}"#)))

    ("Pretty printer"
     
     (

      #-gfx
      (PPRINT nil 1 2 #"
object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  superprint(obj, 0, pfun);
  return symbol(NOTHING);
}"#)

      #+gfx
      (PPRINT nil 1 2 #"
object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
#if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
#endif
  pln(pfun);
  superprint(obj, 0, pfun);
  ppwidth = PPWIDTH;
  return symbol(NOTHING);
}"#)

      #+(and (not gfx) (not code))
    (PPRINTALL nil 0 1 #"
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
      superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(symbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pserial);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  return symbol(NOTHING);
}"#)

    #+(and code (not gfx))
    (PPRINTALL nil 0 1 #"
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
      superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(symbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(symbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pserial);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  return symbol(NOTHING);
}"#)

    #+(and gfx code)
    (PPRINTALL nil 0 1 #"
object *fn_pprintall (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
#if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
#endif
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pfun);
    if (consp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
      superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pfun);
    } else if (consp(val) && car(val)->type == CODE) {
      superprint(cons(symbol(DEFCODE), cons(var, cdr(val))), 0, pfun);
    } else {
      superprint(cons(symbol(DEFVAR), cons(var, cons(quote(val), NULL))), 0, pserial);
    }
    pln(pfun);
    testescape();
    globals = cdr(globals);
  }
  ppwidth = PPWIDTH;
  return symbol(NOTHING);
}"#)))

    ("Format"

     ((FORMAT nil 2 127 #"
void formaterr (object *formatstr, PGM_P string, int p) {
  pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
  indent(p+5, ' ', pserial); pserial('^');
  errorsub(FORMAT, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

object *fn_format (object *args, object *env) {
  (void) env;
  pfun_t pfun = pserial;
  object *output = first(args);
  object *obj;
  if (output == nil) { obj = startstring(FORMAT); pfun = pstr; }
  else if (output != tee) pfun = pstreamfun(args);
  object *formatstr = second(args);
  if (!stringp(formatstr)) error(FORMAT, notastring, formatstr);
  object *save = NULL;
  args = cddr(args);
  int len = stringlength(formatstr);
  int n = 0, width = 0, w, bra = 0;
  char pad = ' ';
  bool tilde = false, comma, quote;
  while (n < len) {
    char ch = nthchar(formatstr, n);
    char ch2 = ch & ~0x20; // force to upper case
    if (tilde) {
      if (comma && quote) { pad = ch; comma = false, quote = false; }
      else if (ch == '\'') {
        if (comma) quote = true; 
        else formaterr(formatstr, PSTR("quote not valid"), n);
      }
      else if (ch == '~') { pfun('~'); tilde = false; }
      else if (ch >= '0' && ch <= '9') width = width*10 + ch - '0';
      else if (ch == ',') comma = true;
      else if (ch == '%') { pln(pfun); tilde = false; }
      else if (ch == '&') { pfl(pfun); tilde = false; }
      else if (ch == '{') {
        if (save != NULL) formaterr(formatstr, PSTR("can't nest ~{"), n);
        if (args == NULL) formaterr(formatstr, noargument, n);
        if (!listp(first(args))) formaterr(formatstr, notalist, n);
        save = args; args = first(args); bra = n; tilde = false;
      }
      else if (ch == '}') {
        if (save == NULL) formaterr(formatstr, PSTR("no matching ~{"), n);
        if (args == NULL) { args = cdr(save); save = NULL; } else n = bra; 
        tilde = false;
      }
      else if (ch2 == 'A' || ch2 == 'S' || ch2 == 'D' || ch2 == 'G' || ch2 == 'X') {
        if (args == NULL) formaterr(formatstr, noargument, n);
        object *arg = first(args); args = cdr(args);
        w = max(width-atomwidth(arg),0); tilde = false;
        if (ch2 == 'A') { prin1object(arg, pfun); indent(w, pad, pfun); }
        else if (ch2 == 'S') { printobject(arg, pfun); indent(w, pad, pfun); }
        else if (ch2 == 'D' || ch2 == 'G') { indent(w, pad, pfun); prin1object(arg, pfun); }
        else if (ch2 == 'X' && integerp(arg)) {
          indent(max(width-hexwidth(arg),0), pad, pfun); pinthex(arg->integer, pfun);
        } else if (ch2 == 'X') { indent(w, pad, pfun); prin1object(arg, pfun); }
        tilde = false;
      } else formaterr(formatstr, PSTR("invalid directive"), n);
    } else {
      if (ch == '~') { tilde = true; pad = ' '; width = 0; comma = false; quote = false; }
      else pfun(ch);
    }
    n++;
  }
  if (output == nil) { obj->cdr = GlobalString; return obj; }
  else return nil;
}"#)))


("LispLibrary"
     
     (
     (REQUIRE nil 1 1 #"
object *fn_require (object *args, object *env) {
  object *arg = first(args);
  object *globals = GlobalEnv;
  if (!symbolp(arg)) error(REQUIRE, notasymbol, arg);
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    if (symbolp(var) && var == arg) return nil;
    globals = cdr(globals);
  }
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    // Is this the definition we want
    int fname = first(line)->name;
    if ((fname == DEFUN || fname == DEFVAR) && symbolp(second(line)) && second(line)->name == arg->name) {
      eval(line, env);
      return tee;
    }
    line = read(glibrary);
  }
  return nil; 
}"#)

     (LISTLIBRARY "list-library" 0 0 #"
object *fn_listlibrary (object *args, object *env) {
  (void) args, (void) env;
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    int fname = first(line)->name;
    if (fname == DEFUN || fname == DEFVAR) {
      pstring(symbolname(second(line)->name), pserial); pserial(' ');
    }
    line = read(glibrary);
  }
  return symbol(NOTHING); 
}"#)))

#+ethernet
    ("Wi-fi"
     
     ((AVAILABLE nil 1 1 #"
object *fn_available (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(AVAILABLE, PSTR("invalid stream"));
  return number(client.available());
}"#)

     (WIFISERVER "wifi-server" 0 0 #"
object *fn_wifiserver (object *args, object *env) {
  (void) args, (void) env;
  server.begin();
  return nil;
}"#)

     #+esp
     (WIFISOFTAP "wifi-softap" 0 4 #"
object *fn_wifisoftap (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  if (args == NULL) return WiFi.softAPdisconnect(true) ? tee : nil;
  object *first = first(args); args = cdr(args);
  if (args == NULL) WiFi.softAP(cstring(first, ssid, 33));
  else {
    object *second = first(args);
    args = cdr(args);
    int channel = 1;
    boolean hidden = false;
    if (args != NULL) { 
      channel = checkinteger(WIFISOFTAP, first(args));
      args = cdr(args);
      if (args != NULL) hidden = (first(args) != nil);
    }
    WiFi.softAP(cstring(first, ssid, 33), cstring(second, pass, 65), channel, hidden);
  }
  return lispstring((char*)WiFi.softAPIP().toString().c_str());
}"#)

     (CONNECTED nil 1 1 #"
object *fn_connected (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(CONNECTED, PSTR("invalid stream"));
  return client.connected() ? tee : nil;
}"#)

     #+esp
     (WIFILOCALIP "wifi-localip" 0 0 #"
object *fn_wifilocalip (object *args, object *env) {
  (void) args, (void) env;
  return lispstring((char*)WiFi.localIP().toString().c_str());
}"#)
     #+riscv
     (WIFILOCALIP "wifi-localip" 0 0 #"
object *fn_wifilocalip (object *args, object *env) {
  (void) args, (void) env;
  Serial.println(WiFi.localIP());
  // return lispstring((char*)WiFi.localIP().toString().c_str());
}"#)

     #+esp
     (WIFICONNECT "wifi-connect" 0 2 #"
object *fn_wificonnect (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  if (args == NULL) { WiFi.disconnect(true); return nil; }
  if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  else WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  int result = WiFi.waitForConnectResult();
  if (result == WL_CONNECTED) return lispstring((char*)WiFi.localIP().toString().c_str());
  else if (result == WL_NO_SSID_AVAIL) error2(WIFICONNECT, PSTR("network not found"));
  else if (result == WL_CONNECT_FAILED) error2(WIFICONNECT, PSTR("connection failed"));
  else error2(WIFICONNECT, PSTR("unable to connect"));
  return nil;
}"#)

     #+riscv
     (WIFICONNECT "wifi-connect" 0 2 #"
object *fn_wificonnect (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  int status = WL_IDLE_STATUS;     // the Wifi radio's status
  // if (args == NULL) { WiFi.disconnect(true); return nil; }
  // if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  while ( status != WL_CONNECTED) {
    status = WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  // int result = WiFi.waitForConnectResult();
  }
  return nil;
}"#)))

    #+gfx
    ("Graphics functions"
     
     ((DRAWPIXEL "draw-pixel" 2 3 #"
object *fn_drawpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(DRAWPIXEL, third(args));
  tft.drawPixel(checkinteger(DRAWPIXEL, first(args)), checkinteger(DRAWPIXEL, second(args)), colour);
  return nil;
#endif
}"#)

     (DRAWLINE "draw-line" 4 5 #"
object *fn_drawline (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWLINE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWLINE, car(args));
  tft.drawLine(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}"#)

     (DRAWRECT "draw-rect" 4 5 #"
object *fn_drawrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(DRAWRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWRECT, car(args));
  tft.drawRect(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}"#)

     (FILLRECT "fill-rect" 4 5 #"
object *fn_fillrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(FILLRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLRECT, car(args));
  tft.fillRect(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}"#)

     (DRAWCIRCLE "draw-circle" 3 4 #"
object *fn_drawcircle (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(DRAWCIRCLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWCIRCLE, car(args));
  tft.drawCircle(params[0], params[1], params[2], colour);
  return nil;
#endif
}"#)

     (FILLCIRCLE "fill-circle" 3 4 #"
object *fn_fillcircle (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(FILLCIRCLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLCIRCLE, car(args));
  tft.fillCircle(params[0], params[1], params[2], colour);
  return nil;
#endif
}"#)

     (DRAWROUNDRECT "draw-round-rect" 5 6 #"
object *fn_drawroundrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(DRAWROUNDRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWROUNDRECT, car(args));
  tft.drawRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
#endif
}"#)

     (FILLROUNDRECT "fill-round-rect" 5 6 #"
object *fn_fillroundrect (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(FILLROUNDRECT, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLROUNDRECT, car(args));
  tft.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
#endif
}"#)

     (DRAWTRIANGLE "draw-triangle" 6 7 #"
object *fn_drawtriangle (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(DRAWTRIANGLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(DRAWTRIANGLE, car(args));
  tft.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  return nil;
#endif
}"#)

     (FILLTRIANGLE "fill-triangle" 6 7 #"
object *fn_filltriangle (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(FILLTRIANGLE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(FILLTRIANGLE, car(args));
  tft.fillTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  return nil;
#endif
}"#)

     (DRAWCHAR "draw-char" 3 6 #"
object *fn_drawchar (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE, bg = COLOR_BLACK, size = 1;
  object *more = cdr(cddr(args));
  if (more != NULL) {
    colour = checkinteger(DRAWCHAR, car(more));
    more = cdr(more);
    if (more != NULL) {
      bg = checkinteger(DRAWCHAR, car(more));
      more = cdr(more);
      if (more != NULL) size = checkinteger(DRAWCHAR, car(more));
    }
  }
  tft.drawChar(checkinteger(DRAWCHAR, first(args)), checkinteger(DRAWCHAR, second(args)), checkchar(DRAWCHAR, third(args)),
    colour, bg, size);
  return nil;
#endif
}"#)

     (SETCURSOR "set-cursor" 2 2 #"
object *fn_setcursor (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setCursor(checkinteger(SETCURSOR, first(args)), checkinteger(SETCURSOR, second(args)));
  return nil;
#endif
}"#)

     (SETTEXTCOLOR "set-text-color" 1 2 #"
object *fn_settextcolor (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  if (cdr(args) != NULL) tft.setTextColor(checkinteger(SETTEXTCOLOR, first(args)), checkinteger(SETTEXTCOLOR, second(args)));
  else tft.setTextColor(checkinteger(SETTEXTCOLOR, first(args)));
  return nil;
#endif
}"#)

     (SETTEXTSIZE "set-text-size" 1 1 #"
object *fn_settextsize (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setTextSize(checkinteger(SETTEXTSIZE, first(args)));
  return nil;
#endif
}"#)

     (SETTEXTWRAP "set-text-wrap" 1 1 #"
object *fn_settextwrap (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setTextWrap(first(args) != NULL);
  return nil;
#endif
}"#)

     (FILLSCREEN "fill-screen" 0 1 #"
object *fn_fillscreen (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(FILLSCREEN, first(args));
  tft.fillScreen(colour);
  return nil;
#endif
}"#)

     (SETROTATION "set-rotation" 1 1 #"
object *fn_setrotation (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.setRotation(checkinteger(SETROTATION, first(args)));
  return nil;
#endif
}"#)

     (INVERTDISPLAY "invert-display" 1 1 #"
object *fn_invertdisplay (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  tft.invertDisplay(first(args) != NULL);
  return nil;
#endif
}"#)

     #-riscv
     (GETPIXEL "get-pixel" 2 2 #"
#if defined(gfxsupport)
uint16_t Technoblogy_ST7735::getPixel (uint16_t x, uint16_t y) {
  uint32_t ret = 0;
  startWrite();
  setAddrWindow(x, y, 1, 1);
  writeCommand(ST77XX_RAMRD);
  pinMode(TFT_MOSI, INPUT);
  pinMode(TFT_SCLK, OUTPUT);
  for (int i=0; i<33; i++) {
    digitalWrite(TFT_SCLK, HIGH);
    ret = ret<<1 | digitalRead(TFT_MOSI);
    digitalWrite(TFT_SCLK, LOW);
  }
  pinMode(TFT_MOSI, OUTPUT);
  endWrite();
  return ((ret & 0xf80000)>>8 | (ret & 0xfc00)>>5 | (ret & 0xf8)>>3);
}
#endif

object *fn_getpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  return number(tft.getPixel(checkinteger(DRAWPIXEL, first(args)), checkinteger(DRAWPIXEL, second(args))));
#endif
}"#)

     #+riscv
     (GETPIXEL "get-pixel" 2 2 #"
object *fn_getpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(GETPIXEL, PSTR("not supported"));
  return nil;
#endif
}"#)

     #-riscv
     (XORPIXEL "xor-pixel" 2 3 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorPixel (uint16_t x, uint16_t y, uint16_t color) {
  uint16_t lastcolor = getPixel(x, y);
  if ((x >= 0) && (x < _width) && (y >= 0) && (y < _height)) {
    startWrite();
    writeCommand(ST77XX_RAMWR);
    SPI_WRITE16(color ^ lastcolor);
    endWrite();
  }
}
#endif

object *fn_xorpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(XORPIXEL, third(args));
  tft.xorPixel(checkinteger(XORPIXEL, first(args)), checkinteger(XORPIXEL, second(args)), colour);
  return nil;
#endif
}"#)

     #+riscv
     (XORPIXEL "xor-pixel" 2 3 #"
object *fn_xorpixel (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(XORPIXEL, PSTR("not supported"));
  return nil;
#endif
}"#)

     #-riscv
     (XORSPRITE "xor-sprite" 4 5 #"
#if defined(gfxsupport)
void Technoblogy_ST7735::xorSprite (uint16_t x, uint16_t y, uint32_t top, uint32_t bottom, uint16_t color) {
  uint16_t row[8];
  uint32_t col = 0;
  bool bit;
  if ((x >= 0) && (x+7 < _width) && (y >= 0) && (y+7 < _height)) {
    for (int yd=0; yd<8; yd++) {
      startWrite();
      setAddrWindow(x, y+yd, 8, 1);
      writeCommand(ST77XX_RAMRD);
      pinMode(TFT_MOSI, INPUT);
      pinMode(TFT_SCLK, OUTPUT);
      for (int i=0; i<9; i++) {
        digitalWrite(TFT_SCLK, HIGH);
        digitalWrite(TFT_SCLK, LOW);
      }
      for (int xd=0; xd<8; xd++) {
        for (int i=0; i<24; i++) {
          digitalWrite(TFT_SCLK, HIGH);
          col = col<<1 | digitalRead(TFT_MOSI);
          digitalWrite(TFT_SCLK, LOW);
        }
        row[xd] = ((col & 0xf80000)>>8 | (col & 0xfc00)>>5 | (col & 0xf8)>>3);
      }
      pinMode(TFT_MOSI, OUTPUT);
      endWrite();
      startWrite();
      writeCommand(ST77XX_RAMWR);
      for (int xd=0; xd<8; xd++) {
        if (yd < 4) bit = top>>(31 - xd - yd*8) & 1;
        else bit = bottom>>(31 - xd - (yd-4)*8) & 1;
        if (bit) SPI_WRITE16(row[xd] ^ color);
        else SPI_WRITE16(row[xd]);
      }
      endWrite();
    }
  }
}
#endif

object *fn_xorsprite (object *args, object *env) {
#if defined(gfxsupport)
  (void) env;
  uint32_t params[4]; uint16_t colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(XORSPRITE, car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(XORSPRITE, car(args));
  tft.xorSprite(params[0], params[1], params[2], params[3], colour);
  return nil;
#endif
}"#)

     #+riscv
     (XORSPRITE "xor-sprite" 4 5 #"
object *fn_xorsprite (object *args, object *env) {
#if defined(gfxsupport)
  (void) args, (void) env;
  error2(XORSPRITE, PSTR("not supported"));
  return nil;
#endif
}"#)))

    #+badge
    ("Lisp Badge plotting"
     
     ((PLOT nil 0 6 #"
void plotsub (uint8_t x, uint8_t y, uint8_t n, int ys[5]) {
  if (y>=0 && y<64) {
    uint8_t grey = 0x0F-n*3;
    uint8_t blob = grey;
    if ((x&1) == 0) { blob = grey<<4; ys[n] = y; }
    else {
      for (int i=0; i<5; i++) {
        if (y == ys[i]) blob = (0x0F-i*3)<<4 | grey; 
      }
    }
    PlotByte(x>>1, y, blob);
  }
}

object *fn_plot (object *args, object *env) {
  int ys[5] = {-1, -1, -1, -1, -1};
  int xaxis = -1, yaxis = -1;
  delay(20);
  ClearDisplay(); // Clear display
  if (args != NULL && integerp(first(args))) { xaxis = checkinteger(PLOT, first(args)); args = cdr(args); }
  if (args != NULL && integerp(first(args))) { yaxis = checkinteger(PLOT, first(args)); args = cdr(args); }
  int nargs = min(listlength(PLOT, args),4);
  for (int x=0; x<256; x++) {
    object *rest = args;
    for (int n=0; n<nargs; n++) {
      object *function = first(rest);
      int y = checkinteger(PLOT, apply(PLOT, function, cons(number(x), NULL), env));
      plotsub(x, y, n+1, ys);
      rest = cdr(rest);
    }
    plotsub(x, yaxis, 0, ys);
    if (x == xaxis) for (int y=0; y<64; y++) plotsub(x, y, 0, ys);
    if ((x&1) != 0) for (int i=0; i<5; i++) ys[i] = -1;
  }
  while (!tstflag(ESCAPE)); clrflag(ESCAPE);
  return symbol(NOTHING);
}"#)

     (PLOT3D nil 0 3 #"
object *fn_plot3d (object *args, object *env) {
  int xaxis = -1, yaxis = -1;
  uint8_t blob;
  delay(20);
  ClearDisplay(); // Clear display
  if (args != NULL && integerp(first(args))) { xaxis = checkinteger(PLOT3D, first(args)); args = cdr(args); }
  if (args != NULL && integerp(first(args))) { yaxis = checkinteger(PLOT3D, first(args)); args = cdr(args); }
  if (args != NULL) {
    object *function = first(args);
    for (int y=0; y<64; y++) {
      for (int x=0; x<256; x++) {
        int z = checkinteger(PLOT3D, apply(PLOT3D, function, cons(number(x), cons(number(y), NULL)), env));        
        if (x == xaxis || y == yaxis) z = 0xF;
        if ((x&1) == 0) blob = z<<4; else blob = blob | (z&0xF);
        PlotByte(x>>1, y, blob);
      }
    }
  }
  while (!tstflag(ESCAPE)); clrflag(ESCAPE);
  return symbol(NOTHING);
}"#)))))