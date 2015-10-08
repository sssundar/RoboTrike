;NAME KPConst
; Functional Spec
; Description: Keypad Constants 
;
; Some useful constants are included:
;  event calling frequencies DBP, SRP, FRP, RPC
;  state bits                STATE_SCANNING,DEBOUNCING,SLOW/FAST REPEAT
;  event identifiers 		 KEYPRESS event, KEYPRESS row/col values, unpressed
; 							  keys expected IO value
; Revision History:
;  12/17/2014 SSundaresh 	branched from Outline for HW5
;  12/31/2014 SSundaresh 	shifted ID:value constants for KP events
; 							to Events.inc
; 							changed repeat rates to all be equal.


; ms Timer interrupts before a key press is deemed debounced
; DeBouncePeriod, fit in a word
DBP  	EQU 	100

; Once debounced, number of ms timer interrupts before we call another press
; if the key is held throughout (slow repeat rate)
; SlowRepeatPeriod, fit in a word 
SRP 	EQU 	100 

; Same deal, but faster repeat rate
; FastRepeatPeriod, fit in a word 
FRP		EQU		100

; Number of slow repeats before we switch to fast repeat rate
; With a ms timer, (1/1000) * [RC*SRP + DBP] ~ # seconds to hold key before 
; it starts repeating at the fast rate
; RePeatCounter, fit in a byte
RPC 		EQU 	3

; Scanning/Debouncing/Calling state bits (arbitrary)
STATE_SCANNING 		    EQU  00B 
STATE_DEBOUNCING 		EQU  01B
STATE_SLOWREPEAT		EQU  10B
STATE_FASTREPEAT 		EQU  11B

