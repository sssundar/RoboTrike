; General_Timers.inc
; Description: 
; This file includes some definitions for the 80188 Timer Controller.
; It assumes the timer interrupt control registers have been addressed already,
; and PCB_BASE should be the Peripheral Control Block base, defined 
; on a project-by-project basis.

; Revision History:
;  11/14/2014 SSundar 	initially written; doesn't include RIU bits

; REGISTER ADDRESSES
TOCON 			EQU PCB_BASE + 56H
T1CON			EQU PCB_BASE + 5EH
T2CON			EQU PCB_BASE + 66H
T0CNT			EQU PCB_BASE + 50H
T1CNT			EQU PCB_BASE + 58H
T2CNT			EQU PCB_BASE + 60H
T0CMPA			EQU PCB_BASE + 52H
T0CMPB			EQU PCB_BASE + 54H
T1CMPA			EQU PCB_BASE + 5AH
T1CMPB			EQU PCB_BASE + 5CH
T2CMPA			EQU PCB_BASE + 62H

; Control Register Bits
TIMER_RESET_BASE 		EQU  0000000000000000B

TIMER_ENABLE			EQU  1000000000000000B
TIMER_INHIBIT			EQU  0100000000000000B
TIMER_INTERRUPT			EQU  0010000000000000B
TIMER_MC_REACHED 		EQU  0000000000100000B

; AND with current TIMER control state to disable writes to enable bit, and
; clear MAXCOUNT_REACHED Bit
TIMER_MC_REACHED_CLEAR	EQU  1011111111011111B

TIMER_RETRIGGER			EQU  0000000000010000B
TIMER_PRESCALER			EQU  0000000000001000B
TIMER_EXTCLOCK			EQU  0000000000000100B

; DUAL MAXCNT MODE, INTERNAL CLOCK, MEANS LOW ON CMPB, HIGH on CMPA
TIMER_DUAL_MAXCNT_MODE	EQU  0000000000000010B
TIMER_CONT_MODE			EQU  0000000000000001B

; INITIALIZATION CLEARS
CLEAR_TIMER_COUNT		EQU  0000H