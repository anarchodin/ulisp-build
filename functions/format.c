#include "ulisp.h"

void formaterr (object *formatstr, PGM_P string, uint8_t p) {
  pln(pserial); indent(4, ' ', pserial); printstring(formatstr, pserial); pln(pserial);
  indent(p+5, ' ', pserial); pserial('^');
  errorsub(FORMAT, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

object *fn_format (object *args, object *env) {
  (void) env;
  pfun_t pfun = pserial;
  object *output = first(args);
  object *obj;
  if (output == nil) { obj = startstring(FORMAT); pfun = pstr; }
  else if (output != tee) pfun = pstreamfun(args);
  object *formatstr = second(args);
  if (!stringp(formatstr)) error(FORMAT, notastring, formatstr);
  object *save = NULL;
  args = cddr(args);
  int len = stringlength(formatstr);
  uint8_t n = 0, width = 0, w, bra = 0;
  char pad = ' ';
  bool tilde = false, comma, quote;
  while (n < len) {
    char ch = nthchar(formatstr, n);
    char ch2 = ch & ~0x20; // force to upper case
    if (tilde) {
      if (comma && quote) { pad = ch; comma = false, quote = false; }
      else if (ch == '\'') {
        if (comma) quote = true; 
        else formaterr(formatstr, PSTR("quote not valid"), n);
      }
      else if (ch == '~') { pfun('~'); tilde = false; }
      else if (ch >= '0' && ch <= '9') width = width*10 + ch - '0';
      else if (ch == ',') comma = true;
      else if (ch == '%') { pln(pfun); tilde = false; }
      else if (ch == '&') { pfl(pfun); tilde = false; }
      else if (ch == '{') {
        if (save != NULL) formaterr(formatstr, PSTR("can't nest ~{"), n);
        if (args == NULL) formaterr(formatstr, noargument, n);
        if (!listp(first(args))) formaterr(formatstr, notalist, n);
        save = args; args = first(args); bra = n; tilde = false;
      }
      else if (ch == '}') {
        if (save == NULL) formaterr(formatstr, PSTR("no matching ~{"), n);
        if (args == NULL) { args = cdr(save); save = NULL; } else n = bra; 
        tilde = false;
      }
      else if (ch2 == 'A' || ch2 == 'S' || ch2 == 'D' || ch2 == 'G' || ch2 == 'X') {
        if (args == NULL) formaterr(formatstr, noargument, n);
        object *arg = first(args); args = cdr(args);
        uint8_t aw = atomwidth(arg);
        if (width < aw) w = 0; else w = width-aw;
        tilde = false;
        if (ch2 == 'A') { prin1object(arg, pfun); indent(w, pad, pfun); }
        else if (ch2 == 'S') { printobject(arg, pfun); indent(w, pad, pfun); }
        else if (ch2 == 'D' || ch2 == 'G') { indent(w, pad, pfun); prin1object(arg, pfun); }
        else if (ch2 == 'X' && integerp(arg)) {
          uint8_t hw = hexwidth(arg); if (width < hw) w = 0; else w = width-hw;
          indent(w, pad, pfun); pinthex(arg->integer, pfun);
        } else if (ch2 == 'X') { indent(w, pad, pfun); prin1object(arg, pfun); }
        tilde = false;
      } else formaterr(formatstr, PSTR("invalid directive"), n);
    } else {
      if (ch == '~') { tilde = true; pad = ' '; width = 0; comma = false; quote = false; }
      else pfun(ch);
    }
    n++;
  }
  if (output == nil) { obj->cdr = GlobalString; return obj; }
  else return nil;
}
