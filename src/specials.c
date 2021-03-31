/***
 * Special Operators
 */

//;; (quote :type :special)
object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, 0x11, args);
  return first(args);
}

//;; (defun :type :special)
object *sp_defun (object *args, object *env) {
  (void) env;
  checkargs(DEFUN, 0x1F, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFUN, notasymbol, var);
  object *val = cons(symbol(LAMBDA), cdr(args));
  object *pair = value(getname(var),GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}

//;; (defvar :type :special)
object *sp_defvar (object *args, object *env) {
  checkargs(DEFVAR, 0x12, args);
  object *var = first(args);
  if (!symbolp(var)) error(DEFVAR, notasymbol, var);
  object *val = NULL;
  args = cdr(args);
  if (args != NULL) { setflag(NOESC); val = eval(first(args), env); clrflag(NOESC); }
  object *pair = value(getname(var), GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  return var;
}

//;; (setq :type :special)
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
}

//;; (loop :type :special)
object *sp_loop (object *args, object *env) {
  object *start = args;
  #if defined(__XTENSA__)
  yield();
  #endif
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
}

//;; (return :type :special)
object *sp_return (object *args, object *env) {
  object *result = eval(tf_progn(args,env), env);
  setflag(RETURNFLAG);
  return result;
}
