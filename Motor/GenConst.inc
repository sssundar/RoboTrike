;NAME GenConst
; Contains general constants useful in many places.


; Size of all our queues is 511 bytes or 255 words
QUEUE_SIZE EQU 512 		; used in queues.inc to initialize an actual 
						; fixedLengthQueue which can hold 
						; (QUEUE_SIZE-1) byte elements.
						; should be of form 2^n

WORD_QUEUE_LEN EQU 255
BYTE_QUEUE_LEN EQU 511


; generate carry flag by shifting this constant one to the left, logically
CARRY_FLAG_CONSTANT EQU 08000H

; General ASCII Constants
; ASCII difference between 'A' and 'a'
ASCIII_UL 	EQU 00100000B

; simple discriminators for the follow groups 
; order matters - check Letter first, then Number, then Sign.
; based on ASCII encodings
; will have isolated valid characters before using these
; will have converted to lowercase before using these
ASCIILetterIdentifier 	EQU  01000000B
ASCIINumberIdentifier 	EQU	 00010000B
ASCIISignIdentifier 	EQU  00100000B

; ASCII Carriage Return CTRL-M
ASCII_CR EQU 00001101B

; Used in converts.in, different num/let/null 
; descriptions for a different use context.
ASCII_NUMBER	EQU 00110000B
ASCII_LETTER	EQU 01000000B
ASCII_NULL		EQU	00H

; ASCII Low Nibble Mask
LOWNIBBLEMASK EQU 00001111B