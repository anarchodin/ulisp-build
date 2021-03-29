//;; (not :min 1 :max 1)
//;; (null :enum "nullfn" :label "fn_not" :min 1 :max 1)
object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}

//;; (cons :min 2 :max 2)
object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}

//;; (consp :min 1 :max 1)
object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}

//;; (atom :min 1 :max 1)
object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}

//;; (listp :min 1 :max 1)
object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}

//;; (symbolp :min 1 :max 1)
object *fn_symbolp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (arg == NULL || symbolp(arg)) ? tee : nil;
}

//;; (boundp :min 1 :max 1)
object *fn_boundp (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error(BOUNDP, notasymbol, var);
  return boundp(var, env) ? tee : nil;
}

//;; (set :enum "setfn" :min 2)
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
}

//;; (streamp :min 1 :max 1)
object *fn_streamp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return streamp(arg) ? tee : nil;
}

//;; (eq :min 2 :max 2)
object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}

//;; (eql :min 2 :max 2)
object *fn_eql (object *args, object *env) {
  (void) env;
  return eql(first(args), second(args)) ? tee : nil;
}
