// Sleep

#if defined(CPU_ATSAMD21)
void WDT_Handler(void) {
  // ISR for watchdog early warning
  WDT->CTRL.bit.ENABLE = 0;        // Disable watchdog
  while(WDT->STATUS.bit.SYNCBUSY); // Sync CTRL write
  WDT->INTFLAG.bit.EW  = 1;        // Clear interrupt flag
}
#endif

void initsleep () {
#if defined(CPU_ATSAMD21)
 // One-time initialization of watchdog timer.

  // Generic clock generator 2, divisor = 32 (2^(DIV+1))
  GCLK->GENDIV.reg = GCLK_GENDIV_ID(2) | GCLK_GENDIV_DIV(4);
  // Enable clock generator 2 using low-power 32KHz oscillator.
  // With /32 divisor above, this yields 1024Hz clock.
  GCLK->GENCTRL.reg = GCLK_GENCTRL_ID(2) |
                      GCLK_GENCTRL_GENEN |
                      GCLK_GENCTRL_SRC_OSCULP32K |
                      GCLK_GENCTRL_DIVSEL;
  while(GCLK->STATUS.bit.SYNCBUSY);
  // WDT clock = clock gen 2
  GCLK->CLKCTRL.reg = GCLK_CLKCTRL_ID_WDT |
                      GCLK_CLKCTRL_CLKEN |
                      GCLK_CLKCTRL_GEN_GCLK2;

  // Enable WDT early-warning interrupt
  NVIC_DisableIRQ(WDT_IRQn);
  NVIC_ClearPendingIRQ(WDT_IRQn);
  NVIC_SetPriority(WDT_IRQn, 0);         // Top priority
  NVIC_EnableIRQ(WDT_IRQn);
#endif
}

void sleep (int secs) {
#if defined(CPU_ATSAMD21)
  WDT->CTRL.reg = 0;                     // Disable watchdog for config
  while(WDT->STATUS.bit.SYNCBUSY);
  WDT->INTENSET.bit.EW   = 1;            // Enable early warning interrupt
  WDT->CONFIG.bit.PER    = 0xB;          // Period = max
  WDT->CONFIG.bit.WINDOW = 0x7;          // Set time of interrupt = 1024 cycles = 1 sec
  WDT->CTRL.bit.WEN      = 1;            // Enable window mode
  while(WDT->STATUS.bit.SYNCBUSY);       // Sync CTRL write

  SysTick->CTRL = 0;                     // Stop SysTick interrupts
  
  while (secs > 0) {
    WDT->CLEAR.reg = WDT_CLEAR_CLEAR_KEY;// Clear watchdog interval
    while(WDT->STATUS.bit.SYNCBUSY);
    WDT->CTRL.bit.ENABLE = 1;            // Start watchdog now!
    while(WDT->STATUS.bit.SYNCBUSY);
    SCB->SCR |= SCB_SCR_SLEEPDEEP_Msk;   // Deepest sleep
    __DSB();
    __WFI();                             // Wait for interrupt
    secs--;
  }
  SysTick->CTRL = 7;                     // Restart SysTick interrupts
#else
  delay(1000*secs);
#endif
}
