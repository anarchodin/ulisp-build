// Check pins

void checkanalogread (int pin) {
#if defined(__MSP430F5529__)
  if (!(pin==2 || pin==6 || (pin>=23 && pin<=28))) error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR5969__)
  if (!(pin==5 || (pin>=11 && pin<=13) || pin==18 || pin==19 || pin==23 || pin==24)) error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR5994__)
  if (!(pin==2 || pin==6 || (pin>=23 && pin<=28) || (pin>=31 && pin<=33) || pin==43 || pin==44 || pin==47 || pin==134 || pin==135))
    error(PSTR("'analogread' invalid pin"));
#elif defined(__MSP430FR6989__)
  if (!(pin==2 || pin==6 || pin==17 || (pin>=23 && pin<=30) || pin==34 || (pin>=128 && pin<=130) || pin==143))
    error(PSTR("'analogread' invalid pin"));
#endif
}

void checkanalogwrite (int pin) {
#if defined(__MSP430F5529__)
  if (!(pin==12 || pin==19 || (pin>=35 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR5969__)
  if (!(pin==12 || pin==19 || (pin>=6 && pin<=15) || pin==18 || pin==19 || 
  pin==21 || pin==22 || pin==26)) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR5994__)
  if (!(pin==19 || (pin>=37 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#elif defined(__MSP430FR6989__)
  if (!(pin==19 || (pin>=37 && pin<=40))) error(PSTR("'analogwrite' invalid pin"));
#endif
}
