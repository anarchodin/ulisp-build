#include "ulisp.h"

object *fn_mapcan (object *args, object *env) {
  return mapcarcan(MAPCAN, args, env, mapcanfun);
}
