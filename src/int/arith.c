//;; (+ :enum "add")
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
}

//;; (- :enum "subtract" :min 1)
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
}

//;; (* :enum "multiply")
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
}

//;; (/ :enum "divide" :min 1)
//;; (truncate :min 1 :max 2 :label "fn_divide")
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
}

//;; (mod :min 2 :max 2)
object *fn_mod (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(MOD, first(args));
  int arg2 = checkinteger(MOD, second(args));
  if (arg2 == 0) error2(MOD, PSTR("division by zero"));
  int r = arg1 % arg2;
  if ((arg1<0) != (arg2<0)) r = r + arg2;
  return number(r);
}

//;; (1+ :enum "oneplus" :min 1 :max 1)
object *fn_oneplus (object *args, object *env) {
  (void) env;
  int result = checkinteger(ONEPLUS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MAX) error2(ONEPLUS, overflow);
  #endif
  return number(result + 1);
}

//;; (1- :enum "oneminus" :min 1 :max 1)
object *fn_oneminus (object *args, object *env) {
  (void) env;
  int result = checkinteger(ONEMINUS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(ONEMINUS, overflow);
  #endif
  return number(result - 1);
}

//;; (abs :min 1 :max 1)
object *fn_abs (object *args, object *env) {
  (void) env;
  int result = checkinteger(ABS, first(args));
  #if defined(checkoverflow)
  if (result == INT_MIN) error2(ABS, overflow);
  #endif
  return number(abs(result));
}

//;; (max :enum "maxfn" :min 1)
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
}

//;; (min :enum "minfn" :min 1)
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
}

//;; (random  :min 1 :max 1)
object *fn_random (object *args, object *env) {
  (void) env;
  int arg = checkinteger(RANDOM, first(args));
  return number(random(arg));
}
