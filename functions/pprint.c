#include "ulisp.h"

object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
#if defined(gfxsupport)
  if (pfun == gfxwrite) ppwidth = GFXPPWIDTH;
#endif
  pln(pfun);
  superprint(obj, 0, pfun);
#if defined(gfxsupport)
  ppwidth = PPWIDTH;
#endif
  return symbol(NOTHING);
}
