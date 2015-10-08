NAME    converts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   converts                                 ;
;                              Conversion Functions                          ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; converts 
; Description: This file contains two procedures:
; Dec2String and Hex2String, written in 80x86/80x88 assembly code. 
; These functions convert a 16 bit binary value into signed decimal and
; unsigned hexadecimal ASCII strings, with null terminators.
; They take the address to write these strings, and the numbers to convert,
; in registers, and have no return values. 
; 
; Input: AX, SI, meaning defined in functional specs, below. 
; Output: None 
; 
; Error Handling:   None
; Known Bugs:       None
; Revision History: 10/24/2014 Sushant Sundaresh implemented base specs
; Revision History: 11/16/2014 SSundaresh       changed grouping, formatting
; Revision History: 12/29/2014 SSundaresh       general include files.

$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE(converts.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP

; Dec2String
; Functional Specification 
; 
; Description:  This function converts the 16-bit binary value 
; 				passed to it to a signed decimal representation, 
;				and writes that result to memory as an ASCII string
;				at a user-specified address, with a null terminator. 
;
; Operation:    The function takes arguments [n, address], where
;				n is passed in a 16 bit register. It figures out
;				the sign of n, writes '+' or '-' to memory depending
;				on the sign, and then sets n to |n|. The code
;				divides n by highest power of 10 present in n, 
;				(e.g. to start with, 10,000, as 16 bits < 40,000), 
;				to get the most significant decimal 'digit' from 0-9,
;				as a 4-bit value. 
;
;				It writes this to file as an ASCII string, then 
; 				takes the remainder of the division as the new n, 
;	 			and iterates until it hits the 1's digit. Then it
;				writes a null terminator and returns.
;
; Arguments: AX - 16 bit binary value, n
;			 SI - the write address value, interpreted as DS:[SI]
; Return Values:    None
;
; Local Variables : Variables described here have ~persistent 
;					meaning in the code that follows.
;		a (SI)		- current address value to write to, in memory
;   	pwr10 (CX)  - current power of 10 to divide by
;		ten (DI)	- a 16 bit 10 to divide pwr10 with
;       n (BX)      - remaining value to write in decimal		
;
; Constants: 		defined in the associated file, converts.inc
;
; Shared Variables: None
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, AX, BX, CX, DX, DI, SI
; Stack Depth:      0 words
;
; Algorithms:       Repeatedly divide by powers of 10
;
; Data Structures:  None
;
; Known Bugs:       None
; Limitations:      Handles only -32768 <= n <= 32767
;					writes leading zeros to string
;
; Revision History:					
;				10/19/2014 SSundaresh wrote spec
;				10/24/2014 SSundaresh wrote, tested assembly

Dec2String	PROC	NEAR
			PUBLIC	Dec2String					
											; n = AX, a = SI
		CMP AX, 0							; SF = sign(n)
		JNS	WRITE_POSITIVE_SIGN				
		;JS	WRITE_NEGATIVE_SIGN				
		
		WRITE_NEGATIVE_SIGN:				; if n<0, write a '-'
			MOV BYTE PTR [SI], '-'			; then set n = |n|
			NEG AX	
			JMP SETUP_LOCAL_VARIABLES
		
		WRITE_POSITIVE_SIGN:				; if n>=0, write a '+' 					
			MOV BYTE PTR [SI], '+'		
			;JMP SETUP_LOCAL_VARIABLES	

		SETUP_LOCAL_VARIABLES:
			INC SI 							; update address a
			MOV BX, AX						; store |n| in BX 
											; set a 16-bit 10
			MOV DI, 10			 			; a 10 for 16-bit division
			MOV CX, INITIAL_PWR10			; pwr10 = 10,000
			;JMP DEC2STRING_WHILE_LOOP		

		DEC2STRING_WHILE_LOOP:
			CMP CX, 0						; end on pwr10 = 0
			JE  DEC2STRING_ENDWHILE			

		DEC2STRING_WHILE_BODY:	
			MOV AX, BX						; AX = n, clear DX 
			MOV DX, 0						; for 16 bit division
			DIV CX							; now AL=floor(n/pwr10)
			MOV BX, DX						; BX = DX = n mod pwr10
			OR AL, ASCII_NUMBER				; AL = AL in ASCII
			MOV DS:[SI], AL					; write ASCII digit
			INC SI 							; update address a
			MOV AX, CX						; AX = pwr10, clear DX			
			MOV DX, 0						; for 16 bit division
			DIV DI							; compute AX = pwr10/10 
			MOV CX, AX						; update pwr10
			JMP DEC2STRING_WHILE_LOOP

		DEC2STRING_ENDWHILE:			
			MOV BYTE PTR [SI], ASCII_NULL	 ; write null terminator
			INC SI 							 ; update address

		CHECKRESULT_DEC:					; debugging breakpoint
											
	RET

Dec2String	ENDP





; NAME    Hex2String

; Hex2String
; Functional Specification 
; 
; Description:
;		This function converts the 16-bit unsigned binary value, n, passed to
;       it, to a 4-digit hexadecimal representation, and writes that result
;		to memory as an ASCII string at a user-specified address, 
;		with a null terminator. 
;
; Operation:  
;		This function iteratively rotates its argument, 'n' to isolate 
;		blocks of 4-bits in turn. It starts with the highest four bits of n,
;		which are rotated to the lowest four bits of n, copied, masked, 
;		and processed. These 4 bit blocks are understood to represent 
;		hexadecimal values. They are converted to ASCII letters from A-F if 
; 		between 10-15, and converted to ASCII digits 0-9 otherwise. 
;		All hexadecimal characters are written to memory, in sequence
;		from highest magnitude to lowest, as expected. The string is 
;		terminated with a null terminator before the program returns.
;
; Arguments:
;	AX - 16 bit unsigned binary value, n, to convert to hexadecimal.
;	SI - the address value (seen as DS:[SI]) to write to in memory 
;
; Return Values:  	None
;
; Local Variables:  
;	address (SI)  		- current address value to write to, in memory
;   shiftCounter (CX)   - a counter for how many hex digits we've processed
;	n (AX)				- initially, the value to convert to hexadecimal,
;							but later, a rotated version of the original,
;							with the lowest four bits our 'current' hex digit
;	ASCII_n (BX)		- copy of the current hex digit from AX 
;
; Shared Variables: None
; Global Variables: None
;
; Input:            None (all arguments passed in registers)
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, AX, BX, CX, SI
; Stack Depth:      0 words
;
; Algorithms:       Use the fact that hex is just 4-blocks of binary to get hex
;					digits in 0-F using shift operations and masking.
; Data Structures:  None
;
; Known Bugs:       None
; Limitations:      leading 0s included in string
;
; Revision History: 10/20/2014 SSundaresh	wrote spec
;					10/24/2014 SSundaresh	wrote, tested code

Hex2String	PROC	NEAR
			PUBLIC	Hex2String												
										; n = AX, a = SI
		MOV CX, 0 						; CX is our shift counter
										; 16 bits to 4 hexits requires 
										; four 4-bit shifts.
		
		HEX2STRING_WHILE_LOOP:			; so we stop when CX = 4
			CMP CX, 4 					
			JE HEX2STRING_ENDWHILE		
										; n = AX has 4x[4 bits] = 4 hex digits
										; we're rotating, first, bits [16-13], 
		HEX2STRING_WHILE_BODY:			; then [12-9], then [8-4], then [3-0]
			ROL AX, 4 					; to the lowest 4-bits of AX

			INC CX  					; remember, we've done CX + 1 shifts

			MOV BX, AX			        ; lets play with our hex digit 
										; without losing our memory of 
										; how far we've rotated AX so far.

			AND BX, F4_MASK				; so make BL copy only this hex digit

			IF_DIGIT_GREATER_THAN_10:   
				CMP BL, 10 				; hex A,B,C,D,E,F >= 10, 
				JL ELSE_HEXDIGIT		; and they should be treated seperately
				;JGE THEN_HEXLETTER					

				THEN_HEXLETTER:
					SUB BL, 9 			; in this case, remove 9 and append
					OR BL, ASCII_LETTER ; an ASCII_LETTER identifier
					JMP ENDIF_DGT10 	

				ELSE_HEXDIGIT:
					OR BL, ASCII_NUMBER	; 0-9 have a different identifier	
					;JMP ENDIF_DGT10

			ENDIF_DGT10:
				MOV BYTE PTR [SI], BL	; write the ASCII byte to memory
				INC SI				    ; and update our address
				JMP HEX2STRING_WHILE_LOOP 

		HEX2STRING_ENDWHILE:				; we're done converting n
			MOV BYTE PTR [SI], ASCII_NULL 	; so write a null terminator
			INC SI 							; increment our address

		CHECKRESULT_HEX:					; set a debugging breakpoint
													
	RET 									; and return!

Hex2String	ENDP



CODE    ENDS



        END
