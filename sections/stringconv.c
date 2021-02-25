char *cstringbuf (object *arg) {
  cstring(arg, SymbolTop, SYMBOLTABLESIZE-(SymbolTop-SymbolTable));
  return SymbolTop;
}

char *cstring (object *form, char *buffer, int buflen) {
  int index = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = getint(form);
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (index >= buflen-1) error2(0, PSTR("no room for string"));
        buffer[index++] = ch;
      }
    }
    form = car(form);
  }
  buffer[index] = '\0';
  return buffer;
}

object *lispstring (char *s) {
  object *obj = myalloc();
  obj->type = STRING;
  char ch = *s++;
  object *head = NULL;
  int chars = 0;
  while (ch) {
    if (ch == '\\') ch = *s++;
    buildstring(ch, &chars, &head);
    ch = *s++;
  }
  obj->cdr = head;
  return obj;
}
