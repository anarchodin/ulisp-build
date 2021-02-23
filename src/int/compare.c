//;; (/= :enum "noteq" :min 1)
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
}

//;; (= :enum "numeq" :min 1)
object *fn_numeq (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(NUMEQ, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(NUMEQ, first(args));
    if (!(arg1 == arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

//;; (< :enum "less" :min 1)
object *fn_less (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(LESS, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(LESS, first(args));
    if (!(arg1 < arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

//;; (<= :enum "lesseq" :min 1)
object *fn_lesseq (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(LESSEQ, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(LESSEQ, first(args));
    if (!(arg1 <= arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

//;; (> :enum "greater" :min 1)
object *fn_greater (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(GREATER, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(GREATER, first(args));
    if (!(arg1 > arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

//;; (>= :enum "greatereq" :min 1)
object *fn_greatereq (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(GREATEREQ, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(GREATEREQ, first(args));
    if (!(arg1 >= arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

//;; (plusp :min 1 :max 1)
object *fn_plusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(PLUSP, first(args));
  if (arg > 0) return tee;
  else return nil;
}

//;; (minusp :min 1 :max 1)
object *fn_minusp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(MINUSP, first(args));
  if (arg < 0) return tee;
  else return nil;
}

//;; (zerop :min 1 :max 1)
object *fn_zerop (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ZEROP, first(args));
  return (arg == 0) ? tee : nil;
}
