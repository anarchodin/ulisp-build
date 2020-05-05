object **place (symbol_t name, object *args, object *env) {
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (issymbol(function, CAR) || issymbol(function, FIRST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take car"), value);
    return &car(value);
  }

  if (issymbol(function, CDR) || issymbol(function, REST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take cdr"), value);
    return &cdr(value);
  }

  if (issymbol(function, NTH)) {
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
  if (issymbol(function, AREF)) {
    object *array = eval(second(args), env);
    if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
    object *dimensions = cddr(array);
    args = cdr(args); 
    int y = 0, x = checkinteger(AREF, eval(second(args), env));
    if (listp(dimensions)) {
      if (cddr(args) == NULL) error2(AREF, PSTR("array needs two subscripts"));
      y = checkinteger(AREF, eval(third(args), env));
    } else if (cddr(args) != NULL) error2(AREF, PSTR("array needs one subscript"));
    return getarray(AREF, array, x, y);
  }
#endif

  error2(name, PSTR("illegal place"));
  return nil;
}