//;; (push :type :special)
object *sp_push (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  checkargs(PUSH, 0x22, args);
  object *item = eval(first(args), env);
  #ifdef ARRAY
  object **loc = place(PUSH, second(args), env, &bit);
  #else
  object **loc = place(PUSH, second(args), env);
  #endif
  push(item, *loc);
  return *loc;
}

//;; (pop :type :special)
object *sp_pop (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  checkargs(POP, 0x11, args);
  #ifdef ARRAY
  object **loc = place(POP, first(args), env, &bit);
  #else
  object **loc = place(POP, first(args), env);
  #endif
  object *result = car(*loc);
  pop(*loc);
  return result;
}

//;; (setf :type :special)
object *sp_setf (object *args, object *env) {
  #ifdef ARRAY
  int bit;
  #endif
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, oddargs);
    #ifdef ARRAY
    object **loc = place(SETF, first(args), env, &bit);
    #else
    object **loc = place(SETF, first(args), env);
    #endif
    arg = eval(second(args), env);
    #ifdef ARRAY
    if (bit == -1) *loc = arg;
    else *loc = number((checkinteger(SETF,*loc) & ~(1<<bit)) | checkbitvalue(SETF,arg)<<bit);
    #else
    *loc = arg;
    #endif
    args = cddr(args);
  }
  return arg;
}
