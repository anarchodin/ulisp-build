//;; (car :min 1 :max 1)
//;; (first :min 1 :max 1 :label "fn_car")
object *fn_car (object *args, object *env) {
  (void) env;
  return carx(first(args));
}

//;; (cdr :min 1 :max 1)
//;; (rest :min 1 :max 1 :label "fn_cdr")
object *fn_cdr (object *args, object *env) {
  (void) env;
  return cdrx(first(args));
}

//;; (caar :min 1 :max 1)
object *fn_caar (object *args, object *env) {
  (void) env;
  return carx(carx(first(args)));
}

//;; (cadr :min 1 :max 1)
//;; (second :min 1 :max 1 :label "fn_cadr")
object *fn_cadr (object *args, object *env) {
  (void) env;
  return carx(cdrx(first(args)));
}

//;; (cdar :min 1 :max 1)
object *fn_cdar (object *args, object *env) {
  (void) env;
  return cdrx(carx(first(args)));
}

//;; (cddr :min 1 :max 1)
object *fn_cddr (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(first(args)));
}

//;; (caaar :min 1 :max 1)
object *fn_caaar (object *args, object *env) {
  (void) env;
  return carx(carx(carx(first(args))));
}

//;; (caadr :min 1 :max 1)
object *fn_caadr (object *args, object *env) {
  (void) env;
  return carx(carx(cdrx(first(args))));
}

//;; (cadar :min 1 :max 1)
object *fn_cadar (object *args, object *env) {
  (void) env;
  return carx(cdrx(carx(first(args))));
}

//;; (caddr :min 1 :max 1)
//;; (third :min 1 :max 1 :label "fn_caddr")
object *fn_caddr (object *args, object *env) {
  (void) env;
  return carx(cdrx(cdrx(first(args))));
}

//;; (cdaar :min 1 :max 1)
object *fn_cdaar (object *args, object *env) {
  (void) env;
  return cdrx(carx(carx(first(args))));
}

//;; (cdadr :min 1 :max 1)
object *fn_cdadr (object *args, object *env) {
  (void) env;
  return cdrx(carx(cdrx(first(args))));
}

//;; (cddar :min 1 :max 1)
object *fn_cddar (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(carx(first(args))));
}

//;; (cdddr :min 1 :max 1)
object *fn_cdddr (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(cdrx(first(args))));
}
