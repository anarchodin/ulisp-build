// Array utilities

int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n;
}

object *buildarray (int n, int s, object *def) {
  int s2 = s>>1;
  if (s2 == 1) { if (n == 2) return cons(def, def); else return cons(def, NULL); }
  else if (n >= s2) return cons(buildarray(s2, s2, def), buildarray(n - s2, s2, def));
  else return cons(buildarray(n, s2, def), nil);
}

object *makearray (int xd, int yd, object *dimensions, object *def) {
  int size = xd * yd;
  object *ptr = myalloc();
  ptr->type = ARRAY;
  object *tree = nil;
  if (size != 0) tree = buildarray(size, max(nextpower2(size), 2), def);
  ptr->cdr = cons(tree, dimensions);
  return ptr;
}

object **getarray (symbol_t name, object *array, int x, int y) {
  object *dimensions = cddr(array);
  int xd, yd = 1;
  xd = first(dimensions)->integer;
  if (cdr(dimensions) != NULL) yd = second(dimensions)->integer;
  int size = xd * yd;
  int index = x * yd + y;
  if (x >= xd || x < 0 || y >= yd || y < 0) error2(name, PSTR("index out of range"));
  int mask = max(nextpower2(size), 2)>>1;
  object **p = &car(cdr(array));
  while (mask) {
    if ((index & mask) == 0) p = &(car(*p)); else p = &(cdr(*p));
    mask = mask>>1;
  }
  return p;
}

object *listtovector (object *list) {
  int xd = listlength(0, list);
  object *array = makearray(xd, 1, cons(number(xd), NULL), NULL);
  int p = 0;
  while (list != NULL) {
    object **loc = getarray(0, array, p++, 0);
    *loc = first(list);
    list = cdr(list);
  }
  return array;
}

object *listto2darray (object *list) {
  int yd, xd = listlength(0, list);
  if (list == NULL) yd = 0;
  else if (listp(first(list))) yd = listlength(0, first(list));
  else error2(0, PSTR("initial contents not 2d array"));
  object *array = makearray(xd, yd, cons(number(xd), cons(number(yd), NULL)), NULL);
  int x = 0;
  while (list != NULL) {
    object *item = first(list);
    int y = 0;
    while (item != NULL) {
      object **loc = getarray(0, array, x, y++);
      *loc = first(item);
      item = cdr(item);
    }
    x++;
    list = cdr(list);
  }
  return array;
}

void printarray (object *array, pfun_t pfun) {
  object *dimensions = cddr(array);
  int xd = first(dimensions)->integer;
  pfun('#');
  if (cdr(dimensions) == NULL) {
    pfun('(');
    for (int x=0; x<xd; x++) {
      if (x) pfun(' ');
      printobject(*getarray(0, array, x, 0), pfun);
    }
  } else {
    int yd = second(dimensions)->integer;
    pfstring(PSTR("2A("), pfun);
    for (int x=0; x<xd; x++) {
      if (x) pfun(' '); pfun('(');
      for (int y=0; y<yd; y++) {
        if (y) pfun(' ');
        printobject(*getarray(0, array, x, y), pfun);
      }
      pfun(')');
    }  
  }
  pfun(')');
}
