; HW4TimerConstants.inc
; Description: 
; This file contains specific constants for HW4, Timer2,
; the keypad debouncing/display mux 1KHz timers,
; and HW6, Timer1 (motor PWM Timer)
;

; Some explanation for the values chosen for Timer 1, for motor PWM 
; Timer 1 is going to be used here to implement PWM for the motors of our
; RoboTrike.

; The minimum PWM frequency is determined by the inertial timescale of our 
; DC motors. The maximum frequency is determined by the LC characteristics 
; of the motor windings, or possibly by the target board clock limit.

; I read online that for most small DC motors, PWM frequencies in the low 
; tens of Hz can work. Lacking specs for the DC motors themselves, 
; I've chosen a ~30Hz cycle, with a PWM duty cycle resolution of 1/160. 
; I also chose 5kHz control updating, or 200us interrupts off Timer1.

; This resolution ensures that the test cases in the HW6Test are passable,
; as these describe duty cycles of O(1/128) so we must have at least that
; resolution.

; Now, the motor control board has no keypad/display routines in my design.
; This is necessary. I measured my keypad code execution time 
; via an oscilloscope and the test code in Parallel_IO_ChipSetup_Tests,
; which is not attached but is in my U:Drive, and uses the 8255 outputs
; to clock functions I've written. Keypad EnqueueEvent calls result in
; 600 us delays, and otherwise Keypad/Display code takes ~100us. 
; If I were to run Timer2 (1ms interrupts), and run Keypad/Display code,
; alongside motor PWM, since timers cannot interrupt each other,
; I would not be able to run my PWM as fast - the minimum PWM cycle would 
; need to be > 600 us to ensure I do not lose interrupts and have
; my PWM wave slowly drift in phase (and have a fluctuating duty cycle
; due to missed interrupts at different points in the wave).

; Therefore, I require that Timer1 be run alone on the motor control board -
; and that timer1 be the highest priority interrupt on that board,
; capable of nesting any lower priority interrupts (e.g. from serial IO).


; Moreover the timer1 EH code must run in considerably less than 100 us.
; I tried 20us and 100us interrupts - it seems my code takes at most 100us
; to run, and otherwise it just never leaves the interrupt loop.
; To give a reasonable buffer of 100us, I chose a 200us interrupt rate for PWM.
; This also ensures we don't lose timer1 interrupts! Measuring this by 
; asking the interrupt handler to output a pulse on start and completion,
; we see that our PWM_Control EH takes ~60-100 us to run, so
; 200us interrupts would leave ~130 us for other code in between - 
; since most of my code takes ~100 us to run, that means,
; later, all motor board ML and serial IO functions would be interleaved
; in there - and would complete somwhat less than the expected serial io 
; baud rate timescales.
;
; Since the motor control board's only other interrupt source is from serial IO
; we simply need to choose the baud rate in HW7 small enough to ensure
; that we can transmit every keypress event from the target board,
; but also not lose bits due to constant interruption from the PWM timer.

; The reason for choosing 1/100 resolution for PWM duty cycles was that
; anything much less than 200us interrupts seemed unreasonable (given our
; base clock rate of 18MHz and the ~size of most of my programs).

; In testing my design, my choices have consequences.
; I cannot test Hw6TEST.obj directly as it uses the keypad.
; Instead I must test each test case manually.

; Revision History:
;  11/14/2014 SSundar 	created
;  12/22/2014 SSundar 	added PWMTimer constants


; Timer1,2 constants to set duty cycle, frequency, and control bits.

; 9.216MHz / 4 is CLKIN,Timers. 
; Thus to get a 1kHZ timer interrupt, we need
SET_TIMER2_MAXCOUNT EQU 2304
; To get a ~200us interrupt off Timer 1, assuming no other timer interrupts
; enabled, we need a 5kHz interrupt or 2304kHz/5kHz = divider ~ 460.
; Since in reality we have chosen a nicer 160-count PWM cycle, with this 5kHz
; interrupt rate, we really have a PWM cycle that lasts 160*200us = 32ms,
; and on the oscilloscope confirms a PWM duty frequency of ~31 Hz in this case.

; The problem is, the oscilloscope shows that the timer interrupts happen
; like clockwork, but the time it takes the event hander to run varies 
; by about 5-10us on short timescales - within a few PWM cycles.
; With a 1 percent duty cycle, that works out to a maximum error 
; of 5 percent, and really speaking an average error much lower even
; in this case. It's just something to be aware of, not to put too much
; stock in 'true resolution' quoted above. We should keep this in mind
; actually and ensure the user can't actually select speeds low enough
; to yield such worst-case duty cycles.
SET_TIMER1_MAXCOUNT EQU 460

; T2CON control bits
; requires General_Timers.inc included as well, and A PCB_Base to be set.
BASE_TIMER2   EQU TIMER_CONT_MODE
ACTIVE_TIMER2 EQU TIMER_ENABLE + TIMER_INHIBIT + TIMER_INTERRUPT + BASE_TIMER2

; T1CON control bits
; Base is just continuous mode, not dual mode, and register in use CMPA by
; default
BASE_TIMER1   EQU TIMER_CONT_MODE 
; Active is with timer/interrupts enabled.
ACTIVE_TIMER1 EQU TIMER_ENABLE + TIMER_INHIBIT + TIMER_INTERRUPT + BASE_TIMER1

; TUCON Bits to set timer priorities and interrupt masking
; requires General_Interrupts.inc to be included as well, and PCB_Base.
TIMERS_INTERRUPT_CONTROL_REG_VALUE EQU PRIORITY1 + UNMASK_INTERRUPT



