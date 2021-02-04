// Table lookup functions

int builtin (char* n) {
  int entry = 0;
  while (entry < ENDKEYWORDS) {
#ifdef NEEDS_PROGMEM
    if (strcasecmp_P(n, (char*)pgm_read_word(&lookup_table[entry].string)) == 0)
#else
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
#endif
      return entry;
    entry++;
  }
  return ENDKEYWORDS;
}

int longsymbol (char *buffer) {
  char *p = SymbolTable;
  int i = 0;
  while (strcasecmp(p, buffer) != 0) {p = p + strlen(p) + 1; i++; }
  if (p == buffer) {
    // Add to symbol table?
    char *newtop = SymbolTop + strlen(p) + 1;
    if (SYMBOLTABLESIZE - (newtop - SymbolTable) < BUFFERSIZE) error2(0, PSTR("symbol table full"));
    SymbolTop = newtop;
  }
#if defined(__AVR__) || defined(__MSP430__)
  if (i > 1535) error2(0, PSTR("too many long symbols"));
#endif
  // TODO: Should also do the check for 32-bit systems, just for consistency...
  return i + MAXSYMBOL; // First number unused by radix40
}

intptr_t lookupfn (symbol_t name) {
#ifdef NEEDS_PROGMEM
  return pgm_read_word(&lookup_table[name].fptr);
#else
  return (intptr_t)lookup_table[name].fptr;
#endif
}

uint8_t getminmax (symbol_t name) {
#ifdef NEEDS_PROGMEM
  return pgm_read_byte(&lookup_table[name].minmax);
#else
  return lookup_table[name].minmax;
#endif  
}

void checkminmax (symbol_t name, int nargs) {
  uint8_t minmax = getminmax(name);
  if (nargs<(minmax >> 4)) error2(name, toofewargs);
  if ((minmax & 0x0f) != 0x0f && nargs>(minmax & 0x0f)) error2(name, toomanyargs);
}

char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
#ifdef NEEDS_PROGMEM
  strcpy_P(buffer, (char *)(pgm_read_word(&lookup_table[name].string)));
#else
  strcpy(buffer, (char *)(lookup_table[name].string));
#endif
  return buffer;
}

char *lookupsymbol (symbol_t name) {
  char *p = SymbolTable;
  int i = name - MAXSYMBOL;
  while (i > 0 && p < SymbolTop) {p = p + strlen(p) + 1; i--; }
  if (p == SymbolTop) return NULL; else return p;
}

void deletesymbol (symbol_t name) {
  char *p = lookupsymbol(name);
  if (p == NULL) return;
  char *q = p + strlen(p) + 1;
  *p = '\0'; p++;
  while (q < SymbolTop) *(p++) = *(q++);
  SymbolTop = p;
}
