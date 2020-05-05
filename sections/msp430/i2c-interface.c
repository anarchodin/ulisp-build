// I2C interface

uint8_t const TWI_SDA_PIN = 10;
uint8_t const TWI_SCL_PIN = 9;

void I2Cinit(bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

inline uint8_t I2Cread() {
  return Wire.read();
}

inline bool I2Cwrite(uint8_t data) {
  return Wire.write(data);
}

bool I2Cstart(uint8_t address, uint8_t read) {
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return true;
}

bool I2Crestart(uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(true) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return error ? false : true;
}

void I2Cstop(uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}
