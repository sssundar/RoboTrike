NAME Parser
; Contains from the point of view of the Motor board,
; ParseSerialChar, which is a finite state machine
; that parses passed characters into serial commands,
; and its helper functions.
;
; Last Revision 12/27/2014 SSundaresh created
; 				12/29/2014 SSundaresh restricted 
;					alphabet for hw9

; Base Constants
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
; Constants for valid characters, state bits
$INCLUDE (Parser.inc)
; Constants for execution calls
$INCLUDE (Motors.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

; When a command is parsed, it must be executed. These
; are the commands to be executed. They are
; testing functions for now, but will be replaced by
; true versions later.
EXTRN SetMotorSpeed:NEAR
EXTRN GetMotorSpeed:NEAR
EXTRN GetMotorDirection:NEAR
EXTRN SetLaser:NEAR
EXTRN GetLaser:NEAR

EXTRN TransmitStatus:NEAR
EXTRN ClearEOFlag:NEAR
EXTRN GetEOFlag:NEAR

; Functional Spec, ParseSerialChar
; Description: this function implements a finite state machine
; that remembers its current state defined by past inputs, where
; the current state is a growing command. Invalid parsed commands
; yield AX = 1 and valid parsed commands are executed in-house,
; and yield a return value of AX = 0. The character to be parsed
; into the growing command is passed by value in AL.
; Operation: The simplest way to describe the operation of this function
; is to write out the state table.
; 
; Ignore the command characters for the moment. All commands have
; a common grammar. If they take numerical arguments, the arguments must
; be be of the form -32768,+32767. This is not strictly in line with
; the specs for executable functions related to these commands, 
; but checking the test code and the command formats, the commands themselves
; when sent over serial must have this restriction. It works out that
; if you want a speed of > 7fff, you just need to use both S and V commands
; instead of just one SetMotorSpeed.

; So, yes, all the various commands have different limits on the numbers, 
; but first parsing whether the condition above is met gets us 95 percent 
; of the way there. Finally checking whether we're in the bounds can be
; abstracted for now. So.. 

; Let CommandCharacter be K in VDFOR and let 
; Bits543 of the StateBits be referenced as 543, and to update their state
; we write 543++. These bits refer to which digit we're considering in
; the argument, out of a maximum of 5 (10000s place).
;
; We're going to parse F, O, R commands till a \CR, expecting no arguments,
; and V, D will be parsed to either a \CR or at most 5 post-sign digits
; whichever comes first. At this point we will not yet be applying
; size cutoffs - it's just the grammar we're parsing here.
;
; The states of the form ArgZero exist to deal with inputs of the form
; K+-00000000... which are condensed to one state, which just tells us
; we've seen A digit, that was 0, so if we see a \CR next, it's ok,
; we did get an argument.
; So, let
; ESURSZFR  = Execute, Status Update, Reset State, ZF Reset.
; CESURSZFR = Check Argument, conditional ESURSZFR, else Error, NotParsing
; where the argument is understood to be 0 if we're in the ArgZero states.
;
;
; Finally, until we reach an ArgZero state, assume there is a dedicated
; 0 state bit, and when we reach it, that bit goes high. Assume when we 
; leave an ArgZero state the bit goes low again.
; Similarly assume there are three bits 543 that only take non-zero values
; for ArgGEQOne states, and there index the argument from digits 0-4 that we 
; are currently waiting for. So, bits543 = 5 in binary implies an error, as
; there is now no way the magnitude of the argument could fit in [0,32768].
; These are understood, below. We increment 543++ for example, but if
; it yields a value >= 5 we would call an error and revert to NoParsing.
; In addition when we increment we also mean we would write the 
; character c in question to a buffer for later conversion to a binary
; encoding, for bounds checking and as an input to command function calls.
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; NotParsing 						Parsing(c)   		 			Error, 		  NotParsing				Error, NotParsing								NotParsing 						1

; Parsing_NoNumbersYet_SignUnclear 	Error, NotParsing 				K=f,o,r Error, NotParsing 				Error, NotParsing 								ESURSZFR 						2
; 																	K=v,d, c=1-9, Parsing_ArgGEQOne (sign clear, no sign)									K=v-r, Error, NotParsing
; 																	K=v,d, c=0,   Parsing_ArgZero (sign clear, no sign)
; 																											K=v,d, 			   Parsing_NoNumbersYet (sign clear)

; All commands that reach the following states require numerical arguments.

; Parsing_ArgZero 		 			Error, NotParsing 				c=0  , Parsing_ArgZero 					Error, NotParsing 								CESURSZFR 						3
; 																	c=1-9, 543++ Parsing_ArgGEQOne 

; Parsing_ArgGEQOne 	 			Error, NotParsing 				c=0-9, 543++ Parsing_ArgGEQOne 			Error, NotParsing								CESURSZFR 					 	4

; Parsing_NoNumbersYet 		 		Error, NotParsing 				c=0  , Parsing_ArgZero 					Error, NotParsing 								Error, NotParsing				5
; 																	c=1-9, 543++ Parsing_ArgGEQOne 
;
; These transitions and related outputs and error checking
; are broken up into helper functions equivalently named. 
;
; Note that all the command-specific checks happen in either Lumped State 2
; or CESURSZFR, so as long as the grammar is maintained, 
; adding commands requires that we change only two functions.
;
; Arguments:        character c, in AL by value
; Return values:    AX = 0 if c did not yield an invalid command state, else 1
; Constants:  		ParsingBit, SignBits, ZeroSeenBit, ArgIndexBits
; Local Variables:   
; 	BL 	ParserStateBits
;   AL  input character, vetted, converted to lowercase        
; Shared Variables: ParserStateBits 
; Global Variables: none
; Registers used:   AX, BX
; Stack depth:      0 words
; Input:       		none
; Output:           none
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs: 		none
; Limitations:   	not visually simple, like macros/lookup tables
; Revision History:
;   12/27/2014 SSundaresh  created
;   12/29/2014 SSundaresh  state table updated for hw9 command set
; Pseudocode
; Check if input is a valid character using ValidateInputCharacter
; If not, ignore - call ReportValid and return.
; If valid, now have it in AL in lowercase (if applicable).
; Split into states, and call appropriate state output/transition functions
; which set the ZF if there was a problem (which they handled).
; Check the zero-flag and if it is set, report a parsing error, and otherwise
; report no parsing error.
ParseSerialChar   PROC NEAR 
                PUBLIC ParseSerialChar
    CALL ValidateInputCharacter 			; c in AL in lowercase if ok
    JZ Report_Valid_Character 				; ZF set, ignorable input
    JNZ Allowed_Character

    Allowed_Character:    
    MOV BX, offset(ParserStateBits)			; identify current state
    MOV BL, DS:[BX]
    
    MOV BH, ParsingBit 						; are we parsing a command
    AND BH, BL 	
    JZ STATE_NotParsing 					; we are not parsing a command

 											; we're parsing a command
    MOV BH, SignBits 						; are we sure of the argument sign
    AND BH, BL 								; yet
    JZ STATE_Parsing_NoNumbersYet_SignUnclear
 											; yes, we're sure of a sign bit
    MOV BH, NoNumbersBit
    AND BH, BL
    JZ STATE_Parsing_NoNumbersYet
 	 										; yes, we've seen at least a digit
 	AND BH, ZeroSeenBit
 	JNZ STATE_Parsing_ArgZero
 	JMP STATE_Parsing_ArgGEQOne

    STATE_NotParsing:
    CALL NotParsing 						; AL contains valid character
    JMP ParserReporting
    
    STATE_Parsing_NoNumbersYet_SignUnclear:
    CALL Parsing_NoNumbersYet_SignUnclear	; AL contains valid character
    JMP ParserReporting

    STATE_Parsing_NoNumbersYet:
    CALL Parsing_NoNumbersYet
    JMP ParserReporting

    STATE_Parsing_ArgGEQOne:
    CALL Parsing_ArgGEQOne 	 			; AL contains valid character
    JMP ParserReporting

    STATE_Parsing_ArgZero:
    CALL Parsing_ArgZero 	 			; AL contains valid character
    JMP ParserReporting

    ParserReporting:
	JZ Report_ParsingError 					
    JNZ Report_Valid_Character

    Report_Valid_Character:
    MOV AX, 0
    JMP Done_Parsing

    Report_ParsingError:
    CALL ResetParserState 					; reset to NotParsing
    MOV AX, 1
    JMP Done_Parsing

    Done_Parsing:
    RET
ParseSerialChar ENDP


; helper functions follow to make ParseSerialChar easier to read

; ValidateInputCharacter
; Description. Takes input character in AL. Checks if allowed, given 
; Valid_Characters code table. If allowed, converts to lowercase
; if the character is an ASCII letter, and regardless returns it in AL.
; If the character is not allowed it returns with the ZF set.
; Operation: This code depends heavily on the ASCII set used to
; transmit commands. That is, the way uppercase letters are identified
; is entirely dependent on us not using other characters on those lines
; as command characters. Be careful.
; Loops through the code table of allowed characters and if it finds a
; match checks if its a letter and uses the ASCII encoding to 
; shift the letter to lowercase if it is uppercase (ASCIII_UL bit not set).
; Returns with the ZF reset by clearing AH and adding a non-zero constant.
; If the character is not found, by default clears AH and ZF is set.
; Arguments: AL, input character c
; Return Values: ZF set if not an allowed character.
; 				 ZF reset if allowed. 
; Constants: Valid_Characters, a code table.
; 			 NUM_VALID_CHARACTERS, size of the code table.
; Local Variables:
; 	BX absolute index in code table
;   AL ASCII character to validate
;   CX, counter so we know when to stop looking
; Registers Used:  AX, BX
; Last Revised: 12/27/2014 SSundaresh created
ValidateInputCharacter PROC NEAR
					PUBLIC ValidateInputCharacter
	MOV CX, 0 							; start at the beginning of
	 									; Valid_Characters
	MOV BX, offset(Valid_Characters) 	; CS:BX is Valid_Characters[0]

	ValidationLoop_ST:
	CMP AL, CS:[BX] 				; is this our character?
	JE FORMAT_CHARACTER 			; if so, format it
	INC CX
	INC BX 							; o/w move to the next
	CMP CX, NUM_VALID_CHARACTERS 	; if we've checked all of Valid_Characters
	JNE ValidationLoop_ST 			; this isn't a valid character.

	INVALID_CHARACTER:
	CALL SetZF
	JMP DONE_PROCESSING_CHARACTER

	FORMAT_CHARACTER:
	MOV BL, ASCIILetterIdentifier	; let's check if the character is
	 								; a letter. 
	AND BL, AL
	JZ ALMOST_DONE_PROCESSING 		; not a letter

									; is a letter
									; is it uppercase?
	MOV BL, ASCIII_UL
	AND BL, AL
	JZ ASCII_Uppercase 				; if that bit isn't set, Uppercase
	JNZ ALMOST_DONE_PROCESSING

	ASCII_Uppercase:
	ADD AL, ASCIII_UL 				; convert letter to lowercase

	ALMOST_DONE_PROCESSING:
	CALL ClearZF
	JMP DONE_PROCESSING_CHARACTER

	DONE_PROCESSING_CHARACTER:
	RET
ValidateInputCharacter ENDP

; SetZF
; Sets the ZF, destroying AH in the process.
; Registers used: AH
SetZF PROC NEAR
      PUBLIC SetZF
   XOR AH, AH 						; set ZF
   RET
SetZF ENDP

; ClearZF 
; Clears the ZF, screwing up AH in the process.
; Registers used: AH
ClearZF PROC NEAR
 		PUBLIC ClearZF
 	XOR AH, AH 						; clear AH
	ADD AH, 1 						; clear ZF
 	RET
ClearZF ENDP

; StoreArgDigit
; Description: Takes input digit character for current command in AL.
; If storing this digit would not set ParserStateBits 543 (argument index)
; greater than Parser_MAX_DIGITS, stores it in the buffer 
; ParserNumericalArgumentBuffer at the index
; specified by bits 543 of the current ParserStateBits. Increments
; these state bits to point to the next open position in the buffer
; and returns ZF reset.
; Otherwise does not modify state bits.
; 
; If adding would overflow buffer, i.e. if bits 543 + 1 > 
; Parser_MAX_DIGITS, sets ZF and doesn't add anything to buffer.
;
; Might fail if ParserStateBits 543 not currently in 0,Parser_MAX_DIGITS-1
;
; Arguments: AL, digit in 0-9 ASCII.
; Return Value: ZF set/reset as described above.
; Registers Used: AX, BX, DI
; Last Revised: 12/28/2014 SSundaresh created
StoreArgDigit PROC NEAR
		PUBLIC StoreArgDigit
	MOV DI, offset(ParserStateBits)			; identify current state
    MOV BL, DS:[DI]
	AND BL, ArgIndexBits
	SHR BL, 3 					; isolate ArgIndexBits and get them to bits 210
	XOR BH, BH 					; in BX
	PUSH BX						; store BX, current relative argument index

	INC BL
	CMP BL, Parser_MAX_DIGITS
	JG ArgBufferOverflow
 									; no overflow, prepare to add arg to buffer
 									; and update index in state bits

	SHL BL, 3 						; get updated index in right position
	MOV BH, DS:[DI]          		; get state bits in BH
	AND BH, ArgIndexMask 			; clear out index bits in BH
	ADD BH, BL  					; replace with updated index bits       
	MOV DS:[DI], BH 		        ; store updated state

	POP BX							; get relative index to store arg
	ADD BX, offset(ParserNumericalArgumentBuffer)
								; BX is now an absolute address in DS
	MOV DS:[BX], AL 			; store AL there
	CALL ClearZF
	JMP StoreArgDigit_DONE

	ArgBufferOverflow:
	POP BX 						; so we don't have mismatched push/pops
	CALL SetZF
	JMP StoreArgDigit_DONE

	StoreArgDigit_DONE:
	RET
StoreArgDigit ENDP

; NotParsing
; Description. 
; This is the state NotParsing. 
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; NotParsing 						Parsing(c)   		 			Error, 		  NotParsing				Error, NotParsing								NotParsing 						1

; Takes input character in AL. The character is
; guaranteed to be in Valid_Characters, which for now
; means it is in 0-9, +-, vdfor, or ASCII_CR. Since whitespace
; has been ignored before this point, seeing ASCII_CR in this state
; is equivalent to having seen a blank line, and seeing 0-9+-
; is invalid, as we by definiton haven't yet seen a command character.

; So, depending on the input character, this function will transition
; the state to either Parsing (with a target CommandCharacter set and
; state bits set), or stay in NotParsing, if a line break is seen,
; or stay in Not Parsing, but report back an error by setting the ZF.
; By default if there is no issue the ZF is reset.

; Arguments: AL, input character c in lowercase
; Return Values: ZF set if parsing error noted
; 				 ZF reset otherwise
; Constants: ASCII_CR, ASCIILetterIdentifier
; 			SBP_NoNumbersYet_SignUnclear
; Local Variables:
;	AL, c character
;   AH masks and state bits
; Registers Used:  AX, DI
; Last Revised: 12/27/2014 SSundaresh created
NotParsing PROC NEAR
			PUBLIC NotParsing
	MOV AH, ASCIILetterIdentifier 						; AL has character c
	AND AH, AL  										; letter will yield > 0
	JNZ State1_CommandID								; letter is command
														; start parsing

	MOV AH, ASCII_CR
	CMP AH, AL 											; either it is a 
														; blank line, or
														; it is an error
	JNE State1_ParseError
	JMP State1_ValidInput 								; stay NotParsing


	State1_ParseError:  								; c was 0-9+-
	CALL SetZF
	JMP State1_Done

	State1_CommandID:
    MOV DI, offset(CommandCharacter)
	MOV DS:[DI], AL
	MOV AH, SBP_NoNumbersYet_SignUnclear
	MOV DI, offset(ParserStateBits)
    MOV DS:[DI], AH
	 													; set new state	
	JMP State1_ValidInput

	State1_ValidInput:
	CALL ClearZF

	State1_Done:
	RET
NotParsing ENDP

; Parsing_NoNumbersYet_SignUnclear
; Description: Takes input c in AL in lowercase. 
; Sets ZF if parse error noted, else clears.
; The states transitions:
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; Parsing_NoNumbersYet_SignUnclear 	Error, NotParsing 				K=f,o,r Error, NotParsing 				Error, NotParsing 								ESURSZFR 						2
; 																	K=v,d, c=1-9, Parsing_ArgGEQOne (sign clear, no sign)									K=s-e, Error, NotParsing
; 																	K=v,d, c=0,   Parsing_ArgZero (sign clear, no sign)
; 																											K=v,d, 			   Parsing_NoNumbersYet (sign clear)
; Arguments: AL, input character c in lowercase
; Return Values: ZF set if parsing error noted
; 				 ZF reset otherwise
; Registers Used: AX, BX (in calls), DI
; Last Revised: 12/27/2014 SSundaresh created
; PseudoCode
; is the input a letter? If so, error.
; is the input a number? If f,o,r commands, error, else transition on c=0, 1-9.
; is the input a sign?   if f,o,r commands, error, else, no issue, 
; 	set sign bit and transition
; is the input a \CR? if f,o,r commands, great, call execute, but if not,
; error. 
;
; Note: by construction of SerialParseChar parse errors (ZFs) force a
; transition to NotParsing, and that ExecuteCommand will reset to NotParsing
; after a valid execution.
Parsing_NoNumbersYet_SignUnclear PROC NEAR
				PUBLIC Parsing_NoNumbersYet_SignUnclear
	MOV AH, ASCIILetterIdentifier 		; identify input class
	AND AH, AL
	JNZ State2_LetterInput

	MOV AH, ASCIINumberIdentifier
	AND AH, AL
	JNZ State2_NumberInput

	MOV AH, ASCIISignIdentifier
	AND AH, AL
	JNZ State2_SignInput

										; only other option is \CR
	State2_ASCIICRInput:
    MOV DI, offset(CommandCharacter)
    MOV AH, DS:[DI]	
	CMP AH, 'f' 						; f command complete
	JE State2_Execute_Command
	CMP AH, 'o'  						; o command complete
	JE State2_Execute_Command 
	CMP AH, 'r'  						; r command complete
	JE State2_Execute_Command 
	JMP State2_ParseError 				; else with \CR input, error.

	State2_LetterInput:
	JMP State2_ParseError 				; multi command per line error

	State2_NumberInput:
    MOV DI, offset(CommandCharacter)
    MOV AH, DS:[DI]
	CMP AH, 'f' 						; f takes no arguments
	JE State2_ParseError
	CMP AH, 'o'  						; o parse error
	JE State2_ParseError
	CMP AH, 'r'  						; r parse error
	JE State2_ParseError
	CMP AL, '0' 						; if number is 0, no Sign, ArgZero
	 									; else 1-9, so noSign, ArgGEQone
	JE State2_Transition_ArgZero_noSign 	
	JMP State2_Transition_ArgGEQOne_noSign 	

	State2_Transition_ArgZero_noSign:
	MOV AH, SBP_ArgZero_noSign
    MOV DI, offset(ParserStateBits)
	MOV DS:[DI], AH        				; set new state
	JMP State2_ValidInput

	State2_Transition_ArgGEQOne_noSign:
	MOV AH, SBP_ArgGEQOne_noSign
	MOV DI, offset(ParserStateBits)
	MOV DS:[DI], AH        				; set new state
	CALL StoreArgDigit 						; digit in AL, stores in
											; ParserNumericalArgumentBuffer
											; increments arg index state bits'
											; ZF implies too many input digits
											; (not possible here)
	JNZ State2_ValidInput
	JZ State2_ParseError
	
	State2_SignInput:
	MOV DI, offset(CommandCharacter)
    MOV AH, DS:[DI]
	CMP AH, 'f' 						; f parse error
	JE State2_ParseError
	CMP AH, 'o'  						; o parse error
	JE State2_ParseError
	CMP AH, 'r'  						; r parse error
	JE State2_ParseError	

	State2_SignedTransition:
	CMP AL, '-'
	JNE State2_Transition_PosSign
	JE State2_Transition_NegSign

	State2_Transition_NegSign:
	MOV AH, SBP_NoNumbersYet_negative
	MOV DI, offset(ParserStateBits)
	MOV DS:[DI], AH        				; set new state
	JMP State2_ValidInput

	State2_Transition_PosSign:
	MOV AH, SBP_NoNumbersYet_positive
	MOV DI, offset(ParserStateBits)
	MOV DS:[DI], AH        				; set new state
	JMP State2_ValidInput

	State2_ParseError: 	
	CALL SetZF
	JMP State2_Done

	State2_Execute_Command:
	CALL ExecuteCommand
	JMP State2_Done

	State2_ValidInput:
	CALL ClearZF
	JMP State2_Done

	State2_Done:
	RET
Parsing_NoNumbersYet_SignUnclear ENDP


; Parsing_ArgZero
; Description: Takes input c in AL in lowercase. 
; Sets ZF if parse error noted, else clears.
; The states transitions:
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; Parsing_ArgZero 		 			Error, NotParsing 				c=0  , Parsing_ArgZero 					Error, NotParsing 								CESURSZFR 						3
; 																	c=1-9, 543++ Parsing_ArgGEQOne
; Arguments: AL, input character c in lowercase
; Return Values: ZF set if parsing error noted
; 				 ZF reset otherwise
; Registers Used:  AX, BX (in calls), DI
; Last Revised: 12/28/2014 SSundaresh created
; PseudoCode.
; Only commands that take arguments get this far.
; is the input a letter? If so, error.
; is the input a number? Transition on c=0, 1-9.
; is the input a sign?   If so, error. 
; is the input a \CR? Call ExecuteCommand, which carries out CESURSZFR.
Parsing_ArgZero PROC NEAR
				PUBLIC Parsing_ArgZero
	MOV AH, ASCIILetterIdentifier 		; identify input class
	AND AH, AL
	JNZ State3_LetterInput

	MOV AH, ASCIINumberIdentifier
	AND AH, AL
	JNZ State3_NumberInput

	MOV AH, ASCIISignIdentifier
	AND AH, AL
	JNZ State3_SignInput

										; only other option is \CR
	State3_ASCIICRInput:
	JMP State3_Execute_Command 	

	State3_LetterInput:
	JMP State3_ParseError 				; multi command per line error

	State3_NumberInput:	
	CMP AL, '0' 						; if number is 0, stay in ArgZero
	 									; else 1-9, transition to ArgGEQOne
	JE State3_ValidInput 	 			; with appropriate sign bit.	
	JMP State3_Transition_ArgGEQOne 	

	State3_Transition_ArgGEQOne:
	MOV BH, SignBits
	MOV DI, offset(ParserStateBits)
	MOV AH, DS:[DI]
	AND BH, AH 					; get sign bits in BH
	MOV AH, SBP_ArgGEQOne
	ADD AH, BH 				 	; full transition state with proper sign
	
	MOV DS:[DI], AH        				; set new state
	CALL StoreArgDigit 						; digit in AL, stores in
											; ParserNumericalArgumentBuffer
											; increments arg index state bits'
											; ZF implies too many input digits
											; (not possible here)
	JNZ State3_ValidInput
	JZ State3_ParseError
	
	State3_SignInput:	
	JMP State3_ParseError

	State3_ParseError: 	
	CALL SetZF
	JMP State3_Done

	State3_Execute_Command:
	CALL ExecuteCommand
	JMP State3_Done

	State3_ValidInput:
	CALL ClearZF
	JMP State3_Done

	State3_Done:
	RET
Parsing_ArgZero ENDP


; Parsing_ArgGEQOne
; Description: Takes input c in AL in lowercase. 
; Sets ZF if parse error noted, else clears.
; The states transitions:
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; Parsing_ArgGEQOne 				Error, NotParsing 				c=0-9, 543++ Parsing_ArgGEQOne 			Error, NotParsing								CESURSZFR 					 	4
;
; Arguments: AL, input character c in lowercase
; Return Values: ZF set if parsing error noted
; 				 ZF reset otherwise
; Registers Used:  AX, BX (in calls)
; Last Revised: 12/28/2014 SSundaresh created
; PseudoCode.
; Only commands that take arguments get this far.
; is the input a letter? If so, error.
; is the input a number? Stay in this state unless bits 543 > Parser_MAX_DIGITS
; is the input a sign?   If so, error. 
; is the input a \CR? Call ExecuteCommand, which carries out CESURSZFR.
Parsing_ArgGEQOne PROC NEAR
				PUBLIC Parsing_ArgGEQOne
	MOV AH, ASCIILetterIdentifier 		; identify input class
	AND AH, AL
	JNZ State4_LetterInput

	MOV AH, ASCIINumberIdentifier
	AND AH, AL
	JNZ State4_NumberInput

	MOV AH, ASCIISignIdentifier
	AND AH, AL
	JNZ State4_SignInput

										; only other option is \CR
	State4_ASCIICRInput:
	JMP State4_Execute_Command 	

	State4_LetterInput:
	JMP State4_ParseError 				; multi command per line error

	State4_NumberInput:			
	CALL StoreArgDigit 						; digit in AL, stores in
											; ParserNumericalArgumentBuffer
											; increments arg index state bits'
											; ZF implies too many input digits
											; (not possible here)
	JNZ State4_ValidInput
	JZ State4_ParseError
	
	State4_SignInput:	
	JMP State4_ParseError

	State4_ParseError: 	
	CALL SetZF
	JMP State4_Done

	State4_Execute_Command:
	CALL ExecuteCommand
	JMP State4_Done

	State4_ValidInput:
	CALL ClearZF
	JMP State4_Done

	State4_Done:
	RET
Parsing_ArgGEQOne ENDP


; Parsing_NoNumbersYet
; Description: Takes input c in AL in lowercase. 
; Sets ZF if parse error noted, else clears.
; The states transitions:
; 	Input Class		 				1 								2 										3 												4
; States\c 			 			 	v-r 							0-9 									+- 												\CR 							Lumped STATE_ID
; Parsing_NoNumbersYet 		 		Error, NotParsing 				c=0  , Parsing_ArgZero 					Error, NotParsing 								Error, NotParsing				5
; 																	c=1-9, 543++ Parsing_ArgGEQOne 
; Arguments: AL, input character c in lowercase
; Return Values: ZF set if parsing error noted
; 				 ZF reset otherwise
; Registers Used:  AX, BX (in calls), DI
; Last Revised: 12/28/2014 SSundaresh created
; PseudoCode.
; Only commands that take arguments get this far.
; is the input a letter? If so, error.
; is the input a number? Transition on c=0, 1-9.
; is the input a sign?   If so, error. 
; is the input a \CR?  	 If so, error.
Parsing_NoNumbersYet PROC NEAR
				PUBLIC Parsing_NoNumbersYet
	MOV AH, ASCIILetterIdentifier 		; identify input class
	AND AH, AL
	JNZ State5_LetterInput

	MOV AH, ASCIINumberIdentifier
	AND AH, AL
	JNZ State5_NumberInput

	MOV AH, ASCIISignIdentifier
	AND AH, AL
	JNZ State5_SignInput

										; only other option is \CR
	State5_ASCIICRInput:
	JMP State5_ParseError

	State5_LetterInput:
	JMP State5_ParseError 				; multi command per line error

	State5_NumberInput:	
	MOV BH, SignBits
    MOV DI, offset(ParserStateBits)
	MOV AH, DS:[DI]	
	AND BH, AH 							; get sign bits in BH

	CMP AL, '0' 						; if number is 0, trans to ArgZero
	 									; else 1-9, transition to ArgGEQOne
						 	 			; with appropriate sign bit.	
	JE State5_Transition_ArgZero
	JMP State5_Transition_ArgGEQOne 	

	State5_Transition_ArgZero:
	MOV AH, SBP_ArgZero
	ADD AH, BH 				 	; full transition state with proper sign
	MOV DS:[DI], AH 				; set new state
	JMP State5_ValidInput

	State5_Transition_ArgGEQOne:	
	MOV AH, SBP_ArgGEQOne
	ADD AH, BH 				 	; full transition state with proper sign

	MOV DS:[DI], AH 				; set new state
	CALL StoreArgDigit 						; digit in AL, stores in
											; ParserNumericalArgumentBuffer
											; increments arg index state bits'
											; ZF implies too many input digits
											; (not possible here)
	JNZ State5_ValidInput
	JZ State5_ParseError
	
	State5_SignInput:	
	JMP State5_ParseError

	State5_ParseError: 	
	CALL SetZF
	JMP State5_Done

	State5_ValidInput:
	CALL ClearZF
	JMP State5_Done

	State5_Done:
	RET
Parsing_NoNumbersYet ENDP

; ExecuteCommand
; Description: Called when a \CR is input to ParseSerialChar 
; and the current parse state is otherwise Parsing and valid. For the current
; CommandCharacter, does bounds-checks on the argument in 
; ParserNumericalArgumentBuffer if the ZeroSeenBit is not set in the
; ParserStateBits. This requires converting the parsed string argument
; into a signed 16-bit binary number using SignedDecString2Bin,
; an internal conversion function.
; 
; If the ZeroSeenBit is set, the argument is understood to be 0.
; If the sign bit is not specified, the argument at this stage is 
; understood to be positive.
;
; If the argument fits within a signed word, this function uses
; the sign bit (+,-,none) to determine which execution function would need
; to be called (absolute, relative settings, for example).

; If relative settings are required, the current MotorBoard state is
; accessed and the desired relative mutation is calculated.

; This net (absolute) setting is bounds-checked, on a command-by-command
; basis. If the argument is within bounds, appropriate execution
; functions are called.

; In all cases where arguments are not in bounds, the ZF is set and 
; the function immediately returns. 

; If the arguments are in bounds, the execution function is called,
; we request that the Motor Board status be transmitted, and we reset the
; parser state to NotParsing to await the next command, then return 
; with the ZF reset.
; 
;
; Note: If this ParseSerialChar is called when Handlers.EOFlag is set,
; this function will not execute any command but R, and will
; return ZF reset for anything else that makes it here.
;
; Shared Variables: CommandCharacter, ParserNumericalArgumentBuffer, 
; 					ParserStateBits
; Arguments: none 
; Return Values: 
; 	ZF set if arguments parsed do not match specified bounds for the CommandChar
; 	ZF reset if command arguments in valid bounds, and command called.
; 	Resets parser state to NotParsing if it executes a valid command.
; Registers Used: AX,BX,CX,DX,SI,DI,flags, considering nested functions
; Last Revised: 12/28/2014 SSundaresh created
; 				12/29/2014 SSundaresh removed S,T,E commands, added R
; 								added error handling state
; 								for parsing after an error.
; 								only R command is recognized then.
ExecuteCommand PROC NEAR
				PUBLIC ExecuteCommand										
    CALL GetEOFlag
    MOV AL, BL 				; store EO flag from Handlers.asm in AL

    MOV SI, offset(CommandCharacter) 
	MOV BL, DS:[SI] 			; first deal with those that don't
								; take arguments
	

	CMP BL, 'r'
	JE EXECUTE_COMMAND_R	

	OR AL, AL 					; is EOF set?
	JNZ EC_ParsingDuringError 	; if so, just return with ZF reset
	JZ EC_NO_EOF_SET
    
    EC_ParsingDuringError:
	CALL ResetParserState
	CALL ClearZF
	JMP EC_Done
    
    EC_NO_EOF_SET:
	CMP BL, 'f'
	JE EXECUTE_COMMAND_F
	CMP BL, 'o'
	JE EXECUTE_COMMAND_O	
	
										; everything else takes an argument
										; with clear sign. before splitting
										; off to deal on a command-by-command
										; basis, do some gross error checking
    MOV SI, offset(ParserStateBits)
	MOV AL, DS:[SI]
	MOV AH, ZeroSeenBit
	AND AH, AL
	JZ ExecuteWithArgGEQOne				; otherwise, argument 0,
										; no need for further error checking
	JMP ExecuteWithArgZero

	ExecuteWithArgGEQOne:
	MOV AH, SignBits
	AND AH, AL 							; get just sign bits in AH
	MOV BH, NegSignBit					; check sign before conversion
										; for initial bounds-checking
										; if not negative, default positive
	CMP AH, BH
	JE BOUNDS_CHECK1_NEGATIVE
	JNE BOUNDS_CHECK1_POSITIVE

	BOUNDS_CHECK1_NEGATIVE:
	MOV BH, 1 							; see SignedDecString2Bin spec
										; BH contains 1 if sign negative
	JMP BOUNDS_CHECK1

	BOUNDS_CHECK1_POSITIVE:
	MOV BH, 0 							; and 0 if sign positive
	JMP BOUNDS_CHECK1

	BOUNDS_CHECK1:
	MOV BL, AL
	AND BL, ArgIndexBits
	SHR BL, 3 							; BL contains n, length of arg string	
	MOV SI, offset(ParserNumericalArgumentBuffer) 
										; SI contains string offset in DS
	CALL SignedDecString2Bin 			; now ZF tells us if there's an OF
										; and BX contains binarized argument
	JZ EC_ParseError
	MOV AX, BX 							; argument was in loosest bounds
										; move to AX and do careful checks
	JMP Execute_ArgumentPassedCheck1

	ExecuteWithArgZero:
	MOV AX, 0
	JMP Execute_ArgumentPassedCheck1 	; want to handle all commands 
										; with same code.

	Execute_ArgumentPassedCheck1: 		; binarized argument in AX
										; get parser state, command char								
										; in 	BL, 			BH
    MOV SI, offset(CommandCharacter)
	MOV BH, DS:[SI]
    MOV SI, offset(ParserStateBits)
	MOV BL, DS:[SI]

	CMP BH, 'v'
	JE EXECUTE_COMMAND_V
	CMP BH, 'd'
	JE EXECUTE_COMMAND_D
		
    EXECUTE_COMMAND_F:
    CALL COMMAND_F
	JMP EC_ValidInput
    EXECUTE_COMMAND_O:	
	CALL COMMAND_0
    JMP EC_ValidInput
    EXECUTE_COMMAND_R:
    CALL COMMAND_R
    JMP EC_ValidInput
     							; previous bounds check is suffient here.
    EXECUTE_COMMAND_V:
    CALL COMMAND_V    
    JMP EC_ValidInput
    EXECUTE_COMMAND_D:
    CALL COMMAND_D
    JMP EC_ValidInput
    
	EC_ValidInput:
	CALL TransmitStatus
	CALL ResetParserState
	CALL ClearZF
	JMP EC_Done

	EC_ParseError: 	
	CALL SetZF
	JMP EC_Done	

	EC_Done:
	RET
ExecuteCommand ENDP

; See Comments and ExecuteCommand for Details on when the following
; are called. They are stub functions, originally part of ExecuteCommand
; but split to avoid far-jump errors.
; Each of the functions below implements the Serial Command Format Spec.  
; For each, below, expect BL = state, AX argument, signed 16bit  
COMMAND_F PROC NEAR
        PUBLIC COMMAND_F
    MOV AX, 1 						; AX nonzero turns laser on
    CALL SetLaser    
    RET
COMMAND_F ENDP
	
COMMAND_0 PROC NEAR
        PUBLIC COMMAND_0
    MOV AX, 0 						; see COMMAND_F
	CALL SetLaser
    RET
COMMAND_0 ENDP

COMMAND_R PROC NEAR
        PUBLIC COMMAND_R
    MOV AX, 0 						; just set speed, direction to 0
    MOV BX, 0 						
	CALL SetMotorSpeed
	CALL ClearEOFlag 				; clear error_occurred flag just 
	 								; in case that's why we saw R
    RET
COMMAND_R ENDP
	
COMMAND_V PROC NEAR
        PUBLIC COMMAND_V
        PUSH AX
		CALL GetMotorSpeed
		MOV BX, AX
		POP AX 							; AX = relative speed in [-32768,32767] 
										; BX = current speed in [0,65534]
                                                                                
                                        ; if we consider TEST11, which
                                        ; is of the form
                                        ; S+32767 = 7fff
                                        ; V+32767 = 7fff
                                        ; Expected Output: Speed fffe.
                                        
                                        ; So, here, we make a decision:
                                        ; If V forces speed > 65534 by adding,
                                        ; we truncate to 65534.
                                        ; If V forces speed < 0 by adding
                                        ; we truncate to 0.
                                        
                                        ; BX can be in [0,fffe] unsigned
                                        ; AX can be in [-8000,7fff] signed
                                        ; so break it up.
                                        ; if BX SF is set, AX > 0 
                                        ;   -BX works out to 2^16-BX >= 2
                                        ; so if AX >= |2^16-BX|-2 
                                        ; we'll hit either ffff or overflow,
                                        ; both of which are bad for us.
                                        ; if BX SF is set, AX < 0
                                        ;   no issue when we just add
                                        ; if BX SF is unset, AX > 0
                                        ;   no issue when we just add
                                        ; if BX SF is unset, AX < 0
                                        ;   if BX + AX < 0 we set to 0 instead
		OR BX, BX               ; set SF by BX
        JS BX_SF_SET
        JNS BX_SF_UNSET
        
        BX_SF_SET:
        CMP AX, 0 
        JLE COMMAND_V_JustAdd
        MOV CX, BX  ; AX > 0
        NEG CX      ; this acts like |2^16-BX| effectively for AX > 0, at least
        MOV DI, 2
        NEG DI
        ADD CX, DI  ; |2^16-BX|-2
        CMP AX, CX
        JGE COMMAND_V_MAX_SPEED
        JL COMMAND_V_JustAdd
                
        BX_SF_UNSET:
        CMP AX, 0
        JGE COMMAND_V_JustAdd           
		JL COMMAND_V_CompareMagnitudes 	; AX < 0, is -AX > BX, will BX+AX < 0?

		COMMAND_V_CompareMagnitudes:
		MOV CX, AX
		NEG CX
		CMP CX, BX
		JLE COMMAND_V_JustAdd
		JG COMMAND_V_Halt 				; negative result, so halt	

		COMMAND_V_JustAdd: 
		ADD AX, BX
		MOV BX, SAME_ANGLE
		CALL SetMotorSpeed
		JMP V_DONE        

		COMMAND_V_Halt:
		MOV AX, 0
		MOV BX, SAME_ANGLE
		CALL SetMotorSpeed
		JMP V_DONE
        
        COMMAND_V_MAX_SPEED:
		MOV AX, MAX_SPEED       
		MOV BX, SAME_ANGLE
		CALL SetMotorSpeed
		JMP V_DONE
        
        V_DONE:        
        RET
COMMAND_V ENDP		
    
COMMAND_D PROC NEAR
        PUBLIC COMMAND_D
										; the spec assumes CW angles
										; but in Motors I made my case for
										; why CCW made more sense given
										; the test cases for HW6 so
										; 'right' and 'left' here mean
										; decrease and increase theta
										; respectively, from my POV.

										; get Current Direction in [0,359]
										; know our angle is of the form
										; [-32768, 32767]
                                        ; don't want to risk overflow,
                                        ; and don't want to risk
                                        ; sending 32768 by accident 
                                        ; (seen as SAME but not from
                                        ; this command's perspective).
										; if positive, subtract 720
										; with no risk of overflow
										; if negative, add 720
										; with no risk of overflow
                                        ; and without changing
                                        ; the direction mod 360.
                                        ; 
										; then just add to Current Direction
										; and rely on
										; SetMotorSpeed(65535, new angle)
                                        ; to calculate the modulus.
		OR AX, AX       ; get SF set
		JS ADD_720
		JNS SUB_720

		ADD_720:
		ADD AX, 720
        JMP COMMANDD_UPDATE_DIRECTION
		SUB_720:
		SUB AX, 720
        JMP COMMANDD_UPDATE_DIRECTION

        COMMANDD_UPDATE_DIRECTION:
		MOV BX, AX 					; store our argument		
		CALL GetMotorDirection
									; AX holds current direction in 0,359
		ADD BX, AX
		MOV AX, SAME_SPEED 			
		CALL SetMotorSpeed		    								
        RET
COMMAND_D ENDP	

; Code Segment Arrays for 
; Numerical String Conversion Constants (internal conversions)

; MaxPositiveArgument
; A code segment array containing the ASCII string '32767'
;
; Author:           SSundaresh
; Last Modified:    12/28/2014

MaxPositiveArgument		LABEL   BYTE
						PUBLIC  MaxPositiveArgument
	DB '3'
	DB '2'
	DB '7'
	DB '6'
	DB '7'
	
; MaxNegativeArgument
; A code segment array containing the ASCII string '32768'
;
; Author:           SSundaresh
; Last Modified:    12/28/2014

MaxNegativeArgument		LABEL   BYTE
						PUBLIC  MaxNegativeArgument
	DB '3'
	DB '2'
	DB '7'
	DB '6'
	DB '8'
    
; SignedDecString2Bin
;
; Note
; I do not believe this function belongs in Converts.asm.
; It is too specific to the problem at hand, and its input
; specifications are strict enough that 
; I'd not likely use it again for another purpose. 
; So this feels more like an internal (private) function to me.
;
; Arguments:
; Takes a string from 1-5 ASCII decimal digits at address DS:SI, passed in SI
; by reference, without leading zeros (i.e. binary magnitude is greater than 0)
; and ordered in index 0..n-1 (n length) from MSD to LSD.
; Takes an intended sign in BH (0 +, 1 -) and 
; takes the string length in BL. 

; Return Values:
; Returns in BX the word equivalent of the 
; signed decimal string if there is no overflow during conversion,
; i.e. if the string is of the form [-32768, +32767], 
; and resets the ZF. Otherwise sets the ZF to signify an overflow.

; Operation
; First, we take advantage of the fact that ASCII 0-9 are numerically
; ordered in their binary encoding as well, to decide whether
; our input string will fit in [-32768, +32767].

; If the string length is =5, we compare char by char with the
; code-strings '32678' (if the sign is negative) or
;  			   '32767' (if the sign is positive) from MSD to LSD.
; The first time our input char is less than those code-strings, 
; we stop, we know we have no overflow. If we reach the end and are
; at most equal to those code-strings, still no issue.
; If in this way, we at some point are greater than the code-strings,
; we will have an overflow so we set the ZF and exit.

; If the string length is <5, we're fine. 

; If we've reached this point we can convert the string input, raw,
; as a positive quantity in [0,32768] and negate it afterwards if
; need be, with no worry about overflow.

; Convert raw characters ASCII to binary by masking high 4 bits of 
; argument characters, which yield binary 0-9 in the low nibble.
; As there are n>0 characters to process, the first has power 
; 10^(n-1), so we can multiply by this. We store this
; then proceed to the next digit, multiply by 10^(n-2), add, store,
; repeat till we're out of input characters.
; The result is negated if the input sign value was 1, and remains
; as is if the input sign value was 0.

; This is returned in BX along with a reset ZF.  

; Constants: MaxPositiveArgument, MaxNegativeArgument code strings
; Local Variables (meaning changes, so some are shown twice):
; First, INIT OVERFLOW CHECk
; SI, address of string of magnitude > 0, no leading 0's, to convert
; BL, string length, n in 1-5 
; BH, intended sign, {0,+}, {1,-}
; then CODE-STRING-COMPARISON
; CX, characters left to compare 
; AL, current character being compared between code-string and input string
; SI, BX, offset addresses in input/code string
; then PWR10-CALCULATION
; 
; then CONVERSION
; SI, absolute index in input string during conversion
; BL, string length, n in 1..5 used as relative index
; CX, current power of 10 for conversion from [10^4..10^0]
; DI, stored sum (intermediate conversion term)
; Registers Used: AX, BX, CX, DX, DI, SI
; Stack Depth: 0 words
;
; Last Revised: 12/28/2014 SSundaresh created
SignedDecString2Bin PROC NEAR
			PUBLIC SignedDecString2Bin
	PUSH BX 					; going to need these saved for 
	PUSH SI	 					; when we actually start converting

								; error checking:

								; see arguments
								; BL in 1..5
	CMP BL, 5 					; if we need to convert less than 4
								; digits, we cannot overflow a word
	JL START_Dec2Bin_CONVERSION
	JMP POSSIBLE_OVERFLOW 		; should only be < or =

	POSSIBLE_OVERFLOW:
	MOV CL, BL
	XOR CH, CH 					; CX now contains n = 5, length of both
								; CS:MaxNegativeArgument and input string
								; in DS:SI
	CMP BH, 0 					; see arguments, + sign intended
	JE CHECK_POSITIVE_OVERFLOW
	JNE CHECK_NEGATIVE_OVERFLOW	

	CHECK_NEGATIVE_OVERFLOW:	
	MOV BX, offset(MaxNegativeArgument)
	JMP COMPARISON_LOOP

	CHECK_POSITIVE_OVERFLOW:
	MOV BX, offset(MaxPositiveArgument)
	JMP COMPARISON_LOOP
	
	COMPARISON_LOOP:
	MOV AL, CS:[BX]
	CMP AL, DS:[SI]
	JG START_Dec2Bin_CONVERSION ; can't overflow, guaranteed
	JL GUARANTEED_OVERFLOW 		; will overflow
	INC SI 						; not yet sure
	INC BX 
	DEC CX
	CMP CX, 0
	JE START_Dec2Bin_CONVERSION ; exact equality between strings - no issue
	JMP COMPARISON_LOOP

	GUARANTEED_OVERFLOW:
	POP SI
	POP BX 						; so stack untouched
	CALL SetZF
	JMP Dec2Bin_Conversion_DONE

	START_Dec2Bin_CONVERSION:
	POP SI
	POP BX 						; restore arguments.. need original BL

	PUSH BX						; calculate initial power of 10, uses BX, store
								; original arguments again
	MOV CL, BL
	XOR CH, CH 					; CX holds string length n
	DEC CX 						; CX holds n - 1
	MOV AX, 1 					; AX holds 1
	XOR DX, DX 					; clear DX for DX:AX word multiplication
								; as pwr10 for n = 4 can be more than a byte
	MOV BX, 10 					; word 10 for word MUL by BX

	PWR10LOOP:
	CMP CX, 0
	JE INIT_PWR10_FOUND	
	MUL BX	 					; will fit in AX, DX as 0:AX <- AX * 10
	DEC CX
	JMP PWR10LOOP

	INIT_PWR10_FOUND: 			; it is in AX
	POP BX 						; restore BX, original argument: BL, n,BH,+0,-1
								; SI: original string address in DS
	XOR DI, DI 					; DI will be our accumulator
	MOV CX, AX					; CX our current power of 10
	
	CONVERSION_LOOP:
	XOR DX, DX					; clear DX for word mul
	MOV AL, DS:[SI]
	XOR AH, AH 			
	AND AL, LOWNIBBLEMASK 		; ASCIIDecimal2Bin conversion of ASCII char

	MUL CX 						; DX <- 0, guaranteed
								; AX <- pwr10*[0-9], no overflow
	ADD DI, AX 					; store in accumulator

	XOR DX, DX 					; prepare to calculate CX = pwr10 / 10
	MOV AX, CX
	MOV CX, 10 					; need word division as can have up to 10000/10
								; = 1000 > byte. no remainders expected.
	DIV CX

	MOV CX, AX 					; pwr10 -> pwr10/10

	INC SI
	DEC BL 
	CMP BL, 0
	JG CONVERSION_LOOP

	CMP BH, 1
	JE NEGATE_CONVERTED 		; BH still contains sign input argument
	JMP PREPARE_TO_OUTPUT 		; positive conversions are already ok

	NEGATE_CONVERTED:
	NEG DI
	JMP PREPARE_TO_OUTPUT

	PREPARE_TO_OUTPUT:
	MOV BX, DI 					; want to output in BX	
	CALL ClearZF
	JMP Dec2Bin_Conversion_DONE

	Dec2Bin_Conversion_DONE:
	RET
SignedDecString2Bin ENDP

; ResetParserState
; Description: clear state bits, i.e. set to Not_Parsing state.
; Operational Notes: we only change state bits outside interrupts
; so this is not critical.
; Registers Used: AX, SI
; Last Revised: 12/27/2014 SSundaresh created
ResetParserState PROC NEAR
		   PUBLIC ResetParserState
	MOV AL, STATE_BITS_NOT_PARSING
    MOV SI, offset(ParserStateBits)
	MOV DS:[SI], AL
	RET
ResetParserState ENDP


; Valid_Characters
; A code segment array containing the valid input characters.
; This effectively says, non-valid characters are ignored (whitespace 
; included)
;
; Author:           SSundaresh
; Last Modified:    12/29/2014 for HW9

Valid_Characters		LABEL   BYTE
						PUBLIC  Valid_Characters
	DB 'v' ; set relative speed
	DB 'V'
	DB 'd' ; set direction
	DB 'D'
	DB 'f' ; fire laser
	DB 'F'
	DB 'o' ; laser off
	DB 'O'
	DB 'r' ; reset motors, direction
	DB 'R'
	DB '0' ; numerical argument digits
	DB '1'
	DB '2'
	DB '3'
	DB '4'
	DB '5'
	DB '6'
	DB '7'
	DB '8'
	DB '9'
	DB '+' ; numerical argument sign
	DB '-'
	DB ASCII_CR 	; carriage return, ctrl-M


CODE ENDS

;Shared Variables
DATA    SEGMENT PUBLIC  'DATA'

; the command character whose argument we're currently trying to parse.
CommandCharacter DB ?

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
ParserStateBits  DB ?

; Store parsed numbers known to be arguments of a command
; indexed by state bits 3-5
ParserNumericalArgumentBuffer DB Parser_MAX_DIGITS DUP (?)
	
DATA    ENDS

END