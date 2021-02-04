// I2C interface

void I2Cinit (bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

int I2Cread () {
  return Wire.read();
}

void I2Cwrite (uint8_t data) {
  Wire.write(data);
}

bool I2Cstart (uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   Wire.beginTransmission(address);
   ok = (Wire.endTransmission(true) == 0);
   Wire.beginTransmission(address);
 }
 else Wire.requestFrom(address, I2CCount);
 return ok;
}

bool I2Crestart (uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(false) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return error ? false : true;
}

void I2Cstop (uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}
