// First, a few symbols.
//;; (|:initial-element| :type :symbol :enum "initialelement")
//;; (|:element-type| :type :symbol :enum "elementtype")
//;; (bit :type :symbol)

//;; (arrayp :min 1 :max 1)
object *fn_arrayp (object *args, object *env) {
  (void) env;
  return arrayp(first(args)) ? tee : nil;
}

//;; (array-dimensions :min 1 :max 1)
object *fn_arraydimensions (object *args, object *env) {
  object *array = first(args);
  if (!arrayp(array)) error(ARRAYDIMENSIONS, PSTR("argument is not an array"), array);
  object *dimensions = cddr(array);
  return (getint(first(dimensions)) < 0) ? cons(number(-getint(first(dimensions))), cdr(dimensions)) : dimensions;
}

//;; (make-array :min 1 :max 5)
object *fn_makearray (object *args, object *env) {
  (void) env;
  object *def = nil;
  bool bitp = false;
  object *dims = first(args);
  if (dims == NULL) error2(MAKEARRAY, PSTR("dimensions can't be nil"));
  else if (atom(dims)) dims = cons(dims, NULL);
  args = cdr(args);
  while (args != NULL && cdr(args) != NULL) {
    object *var = first(args);
    if (issymbol(first(args), INITIALELEMENT)) def = second(args);
    else if (issymbol(first(args), ELEMENTTYPE) && issymbol(second(args), BIT)) bitp = true;
    else error(MAKEARRAY, PSTR("argument not recognised"), var); 
    args = cddr(args);
  }
  if (bitp) {
    if (def == nil) def = number(0);
    else def = number(-checkbitvalue(MAKEARRAY, def)); // 1 becomes all ones
  }
  return makearray(MAKEARRAY, dims, def, bitp);
}

//;; (aref :min 2)
object *fn_aref (object *args, object *env) {
  int bit;
  object *array = first(args);
  if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
  object *loc = *getarray(AREF, array, cdr(args), 0, &bit);
  if (bit == -1) return loc;
  else return number(getint(loc)>>bit & 1);
}
