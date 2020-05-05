// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZERO=0, SYMBOL=2, NUMBER=4, STREAM=6, CHARACTER=8, FLOAT=10, STRING=12, PAIR=14 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM, WIFISTREAM };
