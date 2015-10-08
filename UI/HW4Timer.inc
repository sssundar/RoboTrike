; HW4TimerConstants.inc
; Description: 
; This file contains specific constants for HW4, Timer2,
; the keypad debouncing/display mux 1KHz timers,
;
; Revision History:
;  11/14/2014 SSundar 	created
;  12/22/2014 SSundar 	added PWMTimer constants
;  12/31/2014 SSundar 	reduced to only UI board timer constants
; 						updated priority on UI timer


; Timer2 constants to set duty cycle, frequency, and control bits.

; 9.216MHz / 4 is CLKIN,Timers. 
; Thus to get a 1kHZ timer interrupt, we need
SET_TIMER2_MAXCOUNT EQU 2304

; T2CON control bits
; requires General_Timers.inc included as well, and A PCB_Base to be set.
BASE_TIMER2   EQU TIMER_CONT_MODE
ACTIVE_TIMER2 EQU TIMER_ENABLE + TIMER_INHIBIT + TIMER_INTERRUPT + BASE_TIMER2

; TUCON Bits to set timer priorities and interrupt masking
; requires General_Interrupts.inc to be included as well, and PCB_Base.
; For UI Board, timers need lower priority than Serial IO on INTK
; which is currently set to Priority6.
TIMERS_INTERRUPT_CONTROL_REG_VALUE EQU PRIORITY7 + UNMASK_INTERRUPT



