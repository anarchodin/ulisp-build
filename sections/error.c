// Error handling

/*
  errorsub - used by both the error routines.
  Prints: "Error: 'fname' string", where fname is the name of the Lisp function in which the error occurred.
*/
void errorsub (symbol_t fname, PGM_P string) {
  pfl(pserial); pfstring(PSTR("Error: "), pserial);
  if (fname) {
    pserial('\'');
    pstring(symbolname(fname), pserial);
    pserial('\''); pserial(' ');
  }
  pfstring(string, pserial);
}

/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string: symbol", where symbol is the object generating the error.
*/
void error (symbol_t fname, PGM_P string, object *symbol) {
  errorsub(fname, string);
  pserial(':'); pserial(' ');
  printobject(symbol, pserial);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

/*
  error - prints an error message and reenters the REPL.
  Prints: "Error: 'fname' string"
*/
void error2 (symbol_t fname, PGM_P string) {
  errorsub(fname, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

// Save space as these are used multiple times
const char notanumber[] PROGMEM = "argument is not a number";
const char notaninteger[] PROGMEM = "argument is not an integer";
const char notastring[] PROGMEM = "argument is not a string";
const char notalist[] PROGMEM = "argument is not a list";
const char notasymbol[] PROGMEM = "argument is not a symbol";
const char notproper[] PROGMEM = "argument is not a proper list";
const char toomanyargs[] PROGMEM = "too many arguments";
const char toofewargs[] PROGMEM = "too few arguments";
const char noargument[] PROGMEM = "missing argument";
const char nostream[] PROGMEM = "missing stream argument";
const char overflow[] PROGMEM = "arithmetic overflow";
const char indexnegative[] PROGMEM = "index can't be negative";
const char invalidarg[] PROGMEM = "invalid argument";
const char invalidkey[] PROGMEM = "invalid keyword";
const char invalidpin[] PROGMEM = "invalid pin";
const char resultproper[] PROGMEM = "result is not a proper list";
const char oddargs[] PROGMEM = "odd number of arguments";
