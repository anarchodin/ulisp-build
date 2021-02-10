// Helper functions

bool consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

bool atom (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type < PAIR && type != ZZERO;
}

bool listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}

bool improperp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type < PAIR && type != ZZERO;
}

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
  if (!integerp(obj)) error(name, notaninteger, obj);
  return obj->integer;
}

#ifndef __AVR__
int checkbitvalue (symbol_t name, object *obj) {
  if (!integerp(obj)) error(name, notaninteger, obj);
  int n = obj->integer;
  if (n & ~1) error(name, PSTR("argument is not a bit value"), obj);
  return n;
}
#endif /* __AVR__ */

#ifdef FLOAT
float checkintfloat (symbol_t name, object *obj){
  if (integerp(obj)) return obj->integer;
  if (!floatp(obj)) error(name, notanumber, obj);
  return obj->single_float;
}
#endif

int checkchar (symbol_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return obj->chars;
}

int isstream (object *obj){
  if (!streamp(obj)) error(0, PSTR("not a stream"), obj);
  return obj->integer;
}

int issymbol (object *obj, symbol_t n) {
  return symbolp(obj) && obj->name == n;
}

int keywordp (object *obj) {
  if (!symbolp(obj)) return false;
  symbol_t name = obj->name;
  if (name > ENDKEYWORDS) return false; // No keywords except built-ins.
  return (getminmax(name) == CC_KEYWORD);
}

int checkkeyword (symbol_t name, object *obj) {
  if (!keywordp(obj)) error(name, PSTR("argument is not a keyword"), obj);
  symbol_t kname = obj->name;
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

int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
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
