// Array utilities

int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n<2 ? 2 : n;
}

object *buildarray (int n, int s, object *def) {
  int s2 = s>>1;
  if (s2 == 1) {
    if (n == 2) return cons(def, def);
    else if (n == 1) return cons(def, NULL);
    else return NULL;
  } else if (n >= s2) return cons(buildarray(s2, s2, def), buildarray(n - s2, s2, def));
  else return cons(buildarray(n, s2, def), nil);
}

object *makearray (symbol_t name, object *dims, object *def) {
  int size = 1;
  object *dimensions = dims;
  while (dims != NULL) {
    int d = car(dims)->integer;
    if (d < 0) error2(MAKEARRAY, PSTR("dimension can't be negative"));
    size = size * d;
    dims = cdr(dims);
  }
  object *ptr = myalloc();
  ptr->type = ARRAY;
  object *tree = nil;
  if (size != 0) tree = buildarray(size, nextpower2(size), def);
  ptr->cdr = cons(tree, dimensions);
  return ptr;
}

object **arrayref (object *array, int index, int size) {
  int mask = nextpower2(size)>>1;
  object **p = &car(cdr(array));
  while (mask) {
    if ((index & mask) == 0) p = &(car(*p)); else p = &(cdr(*p));
    mask = mask>>1;
  }
  return p;
}
  
object **getarray (symbol_t name, object *array, object *subs, object *env) {
  int index = 0, size = 1, s;
  object *dims = cddr(array);
  while (dims != NULL && subs != NULL) {
    int d = car(dims)->integer;
    if (env) s = checkinteger(name, eval(car(subs), env)); else s = checkinteger(name, car(subs));
    if (s < 0 || s >= d) error(name, PSTR("subscript out of range"), car(subs));
    size = size * d;
    index = index * d + s;
    dims = cdr(dims); subs = cdr(subs);
  }
  if (dims != NULL) error2(name, PSTR("too few subscripts"));
  if (subs != NULL) error2(name, PSTR("too many subscripts"));
  return arrayref(array, index, size);
}

void rslice (object *array, int size, int slice, object *dims, object *args) {
  int d = first(dims)->integer;
  for (int i = 0; i < d; i++) {
    int index = slice * d + i;
    if (cdr(dims) == NULL) {
      if (args == NULL) error2(0, PSTR("initial contents don't match array type"));
      object **p = arrayref(array, index, size);
      *p = car(args);
    } else rslice(array, size, index, cdr(dims), car(args));
    args = cdr(args);
  }
}

object *readarray (int d, object *args) {
  object *list = args;
  object *dims = NULL; object *head = NULL;
  int size = 1;
  for (int i = 0; i < d; i++) {
    int l = listlength(0, list);
    if (dims == NULL) { dims = cons(number(l), NULL); head = dims; }
    else { cdr(dims) = cons(number(l), NULL); dims = cdr(dims); }
    size = size * l;
    if (list != NULL) list = car(list); 
  }
  object *array = makearray(0, head, NULL);
  rslice(array, size, 0, head, args);
  return array;
}

void pslice (object *array, int size, int slice, object *dims, pfun_t pfun) {
  pfun('(');
  int d = first(dims)->integer;
  for (int i = 0; i < d; i++) {
    if (i) pfun(' ');
    int index = slice * d + i;
    if (cdr(dims) == NULL) {
      printobject(*arrayref(array, index, size), pfun);
    } else pslice(array, size, index, cdr(dims), pfun);
  }
  pfun(')');
}

void printarray (object *array, pfun_t pfun) {
  object *dimensions = cddr(array);
  object *dims = dimensions;
  int size = 1, n = 0;
  while (dims != NULL) { size = size * car(dims)->integer; dims = cdr(dims); n++; }
  pfun('#'); if (n > 1) { pint(n, pfun); pfun('A'); }
  pslice(array, size, 0, dimensions, pfun);
}
