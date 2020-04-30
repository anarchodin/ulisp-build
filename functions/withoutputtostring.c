#include "ulisp.h"

object *sp_withoutputtostring (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHOUTPUTTOSTRING, nostream);
  object *var = first(params);
  object *pair = cons(var, stream(STRINGSTREAM, 0));
  push(pair,env);
  object *string = startstring(WITHOUTPUTTOSTRING);
  object *forms = cdr(args);
  eval(tf_progn(forms,env), env);
  string->cdr = GlobalString;
  GlobalString = NULL;
  return string;
}
