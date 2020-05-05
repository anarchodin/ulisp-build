# Some notes

## AVR and progmem

The AVR's Harvard architecture is the root cause of several headaches, but one
in particular is hilarious: ATmega4809 _is_ an AVR, but exposes the flash memory
in the RAM address space. This means that it uses the same code as non-AVR
platforms. Doing that check in CPP every time would be stupidly noisy, so
instead we do it once, in the AVR header, and define the macro `NEEDS_PROGMEM`
for all _other_ AVRs.

The code that needs to use that macro seems like a prime target for refactoring.
