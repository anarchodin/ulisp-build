// Integer operations

//;; (numberp :min 1 :max 1)
object *fn_numberp (object *args, object *env) {
  (void) env;
  return intp(first(args)) ? tee : nil;
}
