; converts.inc
;
; Description: This file defines constants referenced in converts.asm, 
; to make code repurposing less error-prone.
;
; Revision History: 10/24/2014 Sushant Sundaresh created constants for 
; EE51 hw2 (dec2string, hex2string)
; 			12/29/2014 shifted ascII constants out to GenConst.inc

INITIAL_PWR10 	EQU 10000
SHIFT_COUNT 	EQU 00CH
F4_MASK			EQU 000FH