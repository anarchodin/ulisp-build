#include "ulisp.h"

object *sp_withsdcard (object *args, object *env) {
  #if defined(sdcardsupport)
  object *params = first(args);
  if (params == NULL) error2(WITHSDCARD, nostream);
  object *var = first(params);
  object *filename = eval(second(params), env);
  params = cddr(params);
  SD.begin(SDCARD_SS_PIN);
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(WITHSDCARD, first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    SDpfile = SD.open(MakeFilename(filename), oflag);
    if (!SDpfile) error2(WITHSDCARD, PSTR("problem writing to SD card"));
  } else {
    SDgfile = SD.open(MakeFilename(filename), oflag);
    if (!SDgfile) error2(WITHSDCARD, PSTR("problem reading from SD card"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
  #else
  (void) args, (void) env;
  error2(WITHSDCARD, PSTR("not supported"));
  return nil;
  #endif
}
