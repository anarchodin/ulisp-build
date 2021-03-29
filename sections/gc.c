// Garbage collection

/*
  markobject - recursively marks reachable objects, starting from obj
*/
void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (immediatep(obj)) return; // Ignore non-pointers.
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);

  if ((type & 2) == 0) { // cons with allocated car
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  } else if ((type & 15) != 6) { // cons with immediate car
    obj = cdr(obj);
    goto MARK;
  }

  #ifdef ARRAY
  if (type == ARRAY) {
    obj = cdr(obj);
    goto MARK;
  }
  #endif

  if (type == STRING) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}

/*
  sweep - goes through the workspace freeing objects that have not been marked,
  and unmarks marked objects
*/
void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

/*
  gc - perform garbage collection by calling markobject on each of the pointers to objects in use,
  followed by sweep to free unused objects.
*/
void gc (object *form, object *env) {
  #if defined(printgcs)
  int start = Freespace;
  #endif
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  sweep();
  #if defined(printgcs)
  pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
  #endif
}
