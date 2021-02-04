// Radix 40 encoding

#define MAXSYMBOL 4096000000
#define SYMLENGTH 6

/*
  toradix40 - returns a number from 0 to 39 if the character can be encoded, or -1 otherwise.
*/
int toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+30;
  if (ch == '$') return 27; if (ch == '*') return 28; if (ch == '-') return 29;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+1;
  return -1; // Invalid
}

/*
  fromradix40 - returns the character encoded by the number n.
*/
int fromradix40 (int n) {
  if (n >= 1 && n <= 26) return 'a'+n-1;
  if (n == 27) return '$'; if (n == 28) return '*'; if (n == 29) return '-';
  if (n >= 30 && n <= 39) return '0'+n-30;
  return 0;
}

/*
  pack40 - packs six radix40-encoded characters from buffer and returns a 32 bit number.
*/
int pack40 (char *buffer) {
  int x = 0;
  for (int i=0; i<6; i++) x = x * 40 + toradix40(buffer[i]);
  return x;
}

/*
  valid40 - returns true if the symbol in buffer can be encoded as six radix40-encoded characters.
*/
bool valid40 (char *buffer) {
  for (int i=0; i<6; i++) if (toradix40(buffer[i]) == -1) return false;
  return true;
}

char *symbolname (symbol_t x) {
  if (x < ENDKEYWORDS) return lookupbuiltin(x);
  else if (x >= MAXSYMBOL) return lookupsymbol(x);
  char *buffer = SymbolTop;
  buffer[3] = '\0'; buffer[4] = '\0'; buffer[5] = '\0'; buffer[6] = '\0';
  for (int n=5; n>=0; n--) {
    buffer[n] = fromradix40(x % 40);
    x = x / 40;
  }
  return buffer;
}
