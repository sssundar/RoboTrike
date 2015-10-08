; NAME Parser
; Contains constants for valid characters, state bits
; for the Parser.asm function ParseSerialChar.
; Last revision: 12/27/2014 SSundaresh created
; 				 12/29/2014 SSundaresh updated char validity for
; 					new command list for hw9

; maximum possible number of digits as an argument
; to one of our commands (to be parsed). 
; this number needs to be 5 for our argument-checking
; to work - as we base everything around 'does it fit in a signed word'
Parser_MAX_DIGITS EQU 5

; Number of allowed characters, i.e. size of Valid_Characters table.
; VDFOR0-9+-\CR. upper, lower
NUM_VALID_CHARACTERS EQU 23

; Parser State Bit Masks
; To help determine state quickly from ParserStateBits
;LSB
;0 	 	1, Parsing a command. 0, not parsing, idle.
;1,2 	0,0 	sign character unknown
;		0,1 	-  sign
;		1,0 	+  sign
;		1,1 	unsigned
;3,4,5 	next index in the ParserArgumentBuffer, read as a 3-bit binary number
;6		unused
;7		1: have seen at least one 'starting zero' 
;MSB
		        ;76543210
ParsingBit   EQU 00000001B
SignBits     EQU 00000110B
NegSignBit 	 EQU 00000010B
PosSignBit 	 EQU 00000100B
NoNumbersBit EQU 10111000B 	
ArgIndexBits EQU 00111000B
ArgIndexMask EQU 11000111B
ZeroSeenBit  EQU 10000000B

; base state bits follow - for ArgGEQOne bits 543 will increment
; and should be masked out and shifted for update with ArgIndexBits.
; i.e. STATE_BITS AND ArgIndexBits SHR 3 INC by 1 compared with 5 and 
; possibly SHL by 3 to restore position.
STATE_BITS_NOT_PARSING EQU 00000000B
SBP_NoNumbersYet_SignUnclear EQU ParsingBit
SBP_ArgZero_noSign EQU ParsingBit + SignBits + ZeroSeenBit
SBP_ArgGEQOne_noSign EQU ParsingBit + SignBits 
SBP_NoNumbersYet_negative EQU ParsingBit + NegSignBit 
SBP_NoNumbersYet_positive EQU ParsingBit + PosSignBit 
SBP_ArgZero_negative	EQU ParsingBit + NegSignBit + ZeroSeenBit
SBP_ArgZero_positive	EQU ParsingBit + PosSignBit + ZeroSeenBit
SBP_ArgGEQOne_negative EQU ParsingBit + NegSignBit 
SBP_ArgGEQOne_positive EQU ParsingBit + PosSignBit 
SBP_ArgGEQOne EQU ParsingBit
SBP_ArgZero EQU ParsingBit + ZeroSeenBit


