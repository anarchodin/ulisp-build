// In-place operations

/*
  place - returns a pointer to an object referenced in the second argument of an
  in-place operation such as setf. bit is used to indicate the bit position in a bit array
*/
#ifndef ARRAY
object **place (symbol_t name, object *args, object *env) {
#else
object **place (symbol_t name, object *args, object *env, int *bit) {
#endif
  #ifdef ARRAY
  *bit = -1;
  #endif
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (symbolp(function)) {
    symbol_t fname = getname(function);
    if (fname == CAR || fname == FIRST) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, PSTR("can't take car"), value);
      return &car(value);
    }
    if (fname == CDR || fname == REST) {
      object *value = eval(second(args), env);
      if (!listp(value)) error(name, PSTR("can't take cdr"), value);
      return &cdr(value);
    }
    if (fname == NTH) {
      int index = checkinteger(NTH, eval(second(args), env));
      object *list = eval(third(args), env);
      if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
      while (index > 0) {
        list = cdr(list);
        if (list == NULL) error2(name, PSTR("index to nth is out of range"));
        index--;
      }
      return &car(list);
    }
#ifdef ARRAY
    if (fname == AREF) {
      object *array = eval(second(args), env);
      if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
      return getarray(AREF, array, cddr(args), env, bit);
    }
#endif
  }
  error2(name, PSTR("illegal place"));
  return nil;
}

// Checked car and cdr

object *carx (object *arg) {
  if (!listp(arg)) error(0, PSTR("can't take car"), arg);
  if (arg == nil) return nil;
  return car(arg);
}

object *cdrx (object *arg) {
  if (!listp(arg)) error(0, PSTR("can't take cdr"), arg);
  if (arg == nil) return nil;
  return cdr(arg);
}
