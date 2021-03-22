// Make each type of object

#if UINTPTR_MAX == 65535
#define MAX_FIXNUM ((1 << 12) - 1)
#define MIN_FIXNUM (-(1 << 12))
#else
#define MAX_FIXNUM ((1 << 28) - 1)
#define MIN_FIXNUM (-(1 << 28))
#endif

/*
  number - make an integer object with value n and return it
*/
object *number (int n) {
  if (n > MAX_FIXNUM || n < MIN_FIXNUM) {
    object *ptr = myalloc();
    ptr->type = NUMBER;
    ptr->integer = n;
    return ptr;
  } else {
    return (object *)((n << 3) | 2);
  }
}

#ifdef FLOAT
/*
  makefloat - make a floating point object with value f and return it
*/
object *makefloat (float f) {
  object *ptr = myalloc();
  ptr->type = FLOAT;
  ptr->single_float = f;
  return ptr;
}
#endif

/*
  character - make a character object with value c and return it
*/
object *character (uint8_t c) {
  #if PTRWIDTH == 16
  return (object *)((c << 8) | 126);
  #else
  return (object *)((c << 11) | 1022);
  #endif
}

/*
  cons - make a cons with arg1 and arg2 return it
*/
object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}

/*
  symbol - make a symbol object with value name and return it
*/
object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}

/*
  newsymbol - looks through the workspace for an existing occurrence of symbol name and returns it,
  otherwise calls symbol(name) to create a new symbol.
*/
object *newsymbol (symbol_t name) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (symbolp(obj) && obj->name == name) return obj;
  }
  return symbol(name);
}

#ifdef CODE
/*
  codehead - make a code header object with value entry and return it
*/
object *codehead (int entry) {
  object *ptr = myalloc();
  ptr->type = CODE;
  ptr->integer = entry;
  return ptr;
}
#endif

/*
  stream - make a stream object defined by streamtype and address, and return it
*/
object *stream (uint8_t streamtype, uint8_t address) {
  object *ptr = myalloc();
  ptr->type = STREAM;
  ptr->integer = streamtype<<8 | address;
  return ptr;
}
