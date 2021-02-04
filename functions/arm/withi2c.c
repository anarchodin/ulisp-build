#include "ulisp.h"

object *sp_withi2c (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHI2C, nostream);
  object *var = first(params);
  int address = checkinteger(WITHI2C, eval(second(params), env));
  params = cddr(params);
  if ((address == 0 || address == 1) && params != NULL) {
    address = address * 128 + checkinteger(WITHI2C, eval(first(params), env));
    params = cdr(params);
  }
  int read = 0; // Write
  I2CCount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2CCount = rw->integer;
    read = (rw != NULL);
  }
  // Top bit of address is I2C port
  TwoWire *port = &Wire;
  #if defined(ARDUINO_BBC_MICROBIT_V2) || defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41) || defined(MAX32620)
  if (address > 127) port = &Wire1;
  #endif
  I2Cinit(port, 1); // Pullups
  object *pair = cons(var, (I2Cstart(port, address & 0x7F, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(port, read);
  return result;
}
