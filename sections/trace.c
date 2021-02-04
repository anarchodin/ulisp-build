// Tracing

/*
  tracing - returns a number between 1 and TRACEMAX if name is being traced, or 0 otherwise
*/
bool tracing (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) return i+1;
    i++;
  }
  return 0;
}

/*
  trace - enables tracing of symbol name and adds it to the array TraceFn[].
*/
void trace (symbol_t name) {
  if (tracing(name)) error(TRACE, PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(TRACE, PSTR("already tracing 3 functions"));
}

/*
  untrace - disables tracing of symbol name and removes it from the array TraceFn[].
*/
void untrace (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) { TraceFn[i] = 0; return; }
    i++;
  }
  error(UNTRACE, PSTR("not tracing"), symbol(name));
}
