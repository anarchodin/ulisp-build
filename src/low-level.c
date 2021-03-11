//;; (addr :min 1 :max 1)
object *fn_addr(object *args, object *env) {
  (void) env;
  uintptr_t addr = (uintptr_t)first(args); // NOTE: We're not dereferencing the argument.
  return number((intptr_t)addr); // Unsigned numbers would be good.
}

//;; (peek :min 1 :max 1)
object *fn_peek(object *args, object *env) {
  (void) env;
  uintptr_t addr = checkinteger(PEEK, first(args));
  return number(*(int *)addr);
}

//;; (poke :min 2 :max 2)
object *fn_poke(object *args, object *env) {
  (void) env;
  uintptr_t addr = checkinteger(POKE, first(args));
  object *val = second(args);
  *(int *)addr = checkinteger(POKE, val);
  return val;
}

//;; (dumpimage :min 0 :max 0)
object *fn_dumpimage(object *args, object *env) {
  (void) args, (void) env;
  char tmp[20];
  pfl(pserial);
  sprintf(tmp, "Freelist: %08x, ", (uintptr_t)Freelist);
  pstring(tmp, pserial);
  sprintf(tmp, "GlobalEnv: %08x, ", (uintptr_t)GlobalEnv);
  pstring(tmp, pserial);
  sprintf(tmp, "GCStack: %08x, ", (uintptr_t)GCStack);
  pstring(tmp, pserial);
      
  for (int i=0; i<WORKSPACESIZE; i++) {
    if (i%8 == 0) {
      pfl(pserial);
      sprintf(tmp, "%08x: ", (uintptr_t)&Workspace[i]);
      pstring(tmp, pserial);
    }
    sprintf(tmp, "%08x.%08x ", (uintptr_t)car(&Workspace[i]) , (uintptr_t)cdr(&Workspace[i]));
    pstring(tmp, pserial);
  }
  pfl(pserial);
  return nil;
}
