/*
  incfdecf() - Increments/decrements a place by 'increment', and returns the result.
  Calls place() to get a pointer to the numeric value.
*/
object *incfdecf (symbol_t name, object *args, int increment, object *env) {
  checkargs(name, 0x12, args);
  object **loc = place(name, first(args), env);
  int result = checkinteger(name, *loc);
  args = cdr(args);
  if (args != NULL) increment = checkinteger(name, eval(first(args), env)) * increment;
  #if defined(checkoverflow)
  if (increment < 1) { if (INT_MIN - increment > result) error2(name, overflow); }
  else { if (INT_MAX - increment < result) error2(name, overflow); }
  #endif
  result = result + increment;
  *loc = number(result);
  return *loc;
}

/*
  (incf place [number])
  Increments a place, which should have an numeric value, and returns the result.
  The third argument is an optional increment which defaults to 1.
  Calls incfdecf().
*/
//;; (incf :type :special)
object *sp_incf (object *args, object *env) {
  incfdecf(INCF, args, 1, env);
}

/*
  (decf place [number])
  Decrements a place, which should have an numeric value, and returns the result.
  The third argument is an optional decrement which defaults to 1.
  Calls incfdecf().
*/
//;; (decf :type :special)
object *sp_decf (object *args, object *env) {
  incfdecf(DECF, args, -1, env);
}
