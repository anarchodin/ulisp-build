// Set up workspace

/*
  initworkspace - initialises the workspace into a linked list of free objects
*/
void initworkspace () {
  Freelist = NULL;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
  }
}

/*
  myalloc - returns the first object from the linked list of free objects
*/
object *myalloc () {
  if (Freespace == 0) error2(0, PSTR("no room"));
  object *temp = Freelist;
  Freelist = cdr(Freelist);
  Freespace--;
  return temp;
}

/*
  myfree - adds obj to the linked list of free objects.
  inline makes gc significantly faster
*/
inline void myfree (object *obj) {
  car(obj) = NULL;
  cdr(obj) = Freelist;
  Freelist = obj;
  Freespace++;
}
