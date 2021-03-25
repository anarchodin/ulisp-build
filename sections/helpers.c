// Helper functions

bool consp (object *x) {
  if (x == NULL || immediatep(x)) return false; // NULL or immediate are not conses.
  unsigned int type = x->type;
  return (type & 14) != 6; // Anything that doesn't have a type flag in car is a cons.
}

#define atom(x) (!consp(x))

bool listp (object *x) {
  if (x == NULL) return true; // NIL is the empty list, so true.
  if (immediatep(x)) return false; // Not a list if it's immediate.
  unsigned int type = x->type;
  return (type & 14) != 6; // It's a cons if car isn't a type identifier.
}

// REVIEW: This may need to become a function again.
#define improperp(x) (!listp(x))

object *quote (object *arg) {
  return cons(symbol(QUOTE), cons(arg,NULL));
}

int digitvalue (char d) {
  if (d>='0' && d<='9') return d-'0';
  d = d | 0x20;
  if (d>='a' && d<='f') return d-'a'+10;
  return 16;
}

int checkinteger (symbol_t name, object *obj) {
  if (integerp(obj)) {
    return obj->integer;
  } else if (fixnump(obj)) {
    return (intptr_t)obj>>3; // Tagged integer, three bits go away.
  } else {
    error(name, notaninteger, obj);
    return 0; // Silence compiler warning.
  }
}

#ifndef __AVR__
int checkbitvalue (symbol_t name, object *obj) {
  if (!intp(obj)) error(name, notaninteger, obj);
  int n = getint(obj);
  if (n & ~1) error(name, PSTR("argument is not a bit value"), obj);
  return n;
}
#endif /* __AVR__ */

#ifdef FLOAT
float checkintfloat (symbol_t name, object *obj){
  if (fixnump(obj)) return (intptr_t)obj>>3;
  if (integerp(obj)) return obj->integer;
  if (!floatp(obj)) error(name, notanumber, obj);
  return obj->single_float;
}
#endif

int checkchar (symbol_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return getcharacter(obj);
}

int isstream (object *obj){
  if (!streamp(obj)) error(0, PSTR("not a stream"), obj);
  return obj->integer;
}

// TODO: Replace with version that uses immediate values?
int issymbol (object *obj, symbol_t n) {
  return symbolp(obj) && getname(obj) == n;
}

int keywordp (object *obj) {
  if (!symbolp(obj)) return false;
  symbol_t name = getname(obj);
  if (name > ENDKEYWORDS) return false; // No keywords except built-ins.
  return (getcallc(name) == CC_KEYWORD);
}

int checkkeyword (symbol_t name, object *obj) {
  if (!keywordp(obj)) error(name, PSTR("argument is not a keyword"), obj);
  symbol_t kname = getname(obj);
  uintptr_t data = (uintptr_t)lookupfn(kname);
  uint8_t context = data & 0xFF;
  if (context != 0 && context != name) error(name, invalidkey, obj);
  return (data>>8);
}

void checkargs (symbol_t name, uint8_t callc, object *args) {
  // HACK: Uses fixnum encoding to accept both lists and a count.
  // FIXME: This can be made much clearer with full fixnum support.
  if (callc & 0x80) error(0, notvalid, symbol(name));
  int nargs = ((uintptr_t)args & 6) == 2 ? (uintptr_t)args>>3 : listlength(name, args);
  if (nargs<(callc >> 4)) error2(name, toofewargs);
  if ((callc & 0x0f) != 0x0f && nargs>(callc & 0x0f)) error2(name, toomanyargs);
}

int eq (object *a, object *b) {
  return (a == b) ? true : false;
}

int eql (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if (immediatep(arg1) && immediatep(arg2)) return false; // Immediates are not eq if different objects.
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (intp(arg1) && intp(arg2)) return getint(arg1) == getint(arg2); // Equal integers?
  if (immediatep(arg1) || immediatep(arg2)) return false; // Not eq if either is immediate.
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  #ifdef FLOAT
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  #endif
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}

int listlength (symbol_t name, object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(name, notproper);
    list = cdr(list);
    length++;
  }
  return length;
}

void testescape () {
#if !defined(LISPBADGE) || defined(serialmonitor)
  if (Serial.read() == '~') error2(0,PSTR("Escape!"));
#endif
}
