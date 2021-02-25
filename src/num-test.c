//;; (integerp :min 1 :max 1)
object *fn_integerp (object *args, object *env) {
  (void) env;
  return intp(first(args)) ? tee : nil;
}

//;; (fixnump :min 1 :max 1)
object *fn_fixnump (object *args, object *env) {
  (void) env;
  return fixnump(first(args)) ? tee : nil;
}

//;; (oddp :min 1 :max 1)
object *fn_oddp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ODDP, first(args));
  return ((arg & 1) == 1) ? tee : nil;
}

//;; (evenp :min 1 :max 1)
object *fn_evenp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(EVENP, first(args));
  return ((arg & 1) == 0) ? tee : nil;
}
