NAME KPFuncs
; Functional Spec
; Description: Keypad Functions 
;
; Contains the functions:
;  
; KeypadEH (scan for key presses, debounce, then use CallAKeyPress to call 
; presses with a variable repeat rate)
;
; KPInit (initialize our scan/debounce key value/counter variables)
; 
; and the helper functions:
;
; SetKPInterruptCount (set the interrupt count for the next keypad 
; processing state)
;
; DEC_InterruptCount (decrement the interrupt count variable)
;
; CallAKeyPress (set up a call to EnqueueEvent given the observed key press)
;
; Revision History:
;  12/15/2014 SSundaresh planned, wrote spec
;  12/16/2014 SSundaresh added helper functions and generalized keypress
; 						 value constants and row addressing.
;  12/17/2014 SSundaresh wrote pseudocode, wrote code, tested.
;  12/18/2014 SSundaresh forgot to add rowvalue to keyvalue, debugging
;  12/31/2014 SSundaresh updated constants

;First, some useful constants are included:
;  event calling frequencies DBP, SRP, FRP, RPC
;  state bits                STATE_SCANNING,DEBOUNCING,SLOW/FAST REPEAT
;  event identifiers 		 KEYPRESS event, KEYPRESS row/col values, unpressed
; 							  keys expected IO value
;  value/address array sizes numRows, numSupportedKeyPresses
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$include(KPConst.inc)

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'
	ASSUME CS: CGROUP, DS:DGROUP

; EnqueueEvent enqueues a key press event in the UI board command queue.
; For now it is just as a test function called by CallAKeyPress
; when debouncing completes, or when slow/fast repeat cycle.
EXTRN EnqueueEvent:NEAR

; persistent array constants
	
; keypad row IO addresses indexed 0..numRows-1
rowAddressArray1D 		LABEL BYTE
						PUBLIC rowAddressArray1D 
	DB ROW0_IO_ADDRESS
	DB ROW1_IO_ADDRESS
	DB ROW2_IO_ADDRESS
	DB ROW3_IO_ADDRESS

; keypad row press event values indexed 0..numRows-1
rowValuesArray1D 		LABEL BYTE
						PUBLIC rowValuesArray1D
	DB ROW0VAL
	DB ROW1VAL
	DB ROW2VAL
	DB ROW3VAL

; keypad column press event values indexed 0..numSupportedKeyPresses-1
keyPressValuesArray1D 	LABEL BYTE
						PUBLIC keyPressValuesArray1D
	DB COL_LEFT_VAL
	DB COL_MIDLEFT_VAL
	DB COL_MIDRIGHT_VAL
	DB COL_RIGHT_VAL 	


; Functional Spec: KPInit
; Description: Initialize
; 					 	  CURRENT_ROW (0), 
; 					  and CURRENT_PROCESS_STATE (STATE_SCANNING).
;  		Should be called before timer interrupts are enabled.
; Operation: 
; Arguments: none
; Return Values: none
; Local Variables: none with persistent meaning.
; Constants: STATE_SCANNING
; Shared Variables: INTERRUPT_COUNT, CURRENT_ROW, CURRENT_PROCESS_STATE
; Global Variables: none
; Input: none
; Output: none
; Error Handling: none
; Registers Used: AX, BX
; Stack Depth: 2 words
; Algorithms: none
; Data Structures: none
; Known Bugs: none
; Limitations: none
; Revision History:
; 	12/17/14 SSundaresh created

; Pseudocode
;  Push used registers
;  Clear CURRENT_ROW
;  Set CURRENT_PROCESS_STATE to STATE_SCANNING
;  Pop used registers
;  Return

KPInit	PROC NEAR
		PUBLIC KPInit
	PUSH AX  			; save registers AX, BX
	PUSH BX
	
	XOR AH, AH 			; clear AH

						;Initialize CURRENT_ROW byte to 0
	
	MOV BX, offset(CURRENT_ROW)						
	MOV DS:[BX], AH  		; byte cleared

						;Initialize CURRENT_PROCESS_STATE to STATE_SCANNING
	MOV AL, STATE_SCANNING
    MOV BX, offset(CURRENT_PROCESS_STATE)
    MOV DS:[BX], AL     ; state bits set to STATE_SCANNING

	POP BX
	POP AX 				; restore registers AX, BX
	RET
KPInit ENDP 

; Functional Spec: KeypadEH
; Description: This function will be called by a ms Timer Interrutp event
; handler elsewhere. It assumes chip-selects for the keypad are already
; set up.
; 
; The purpose of this function is to switch between a keypad-row scanning,
; keypad key press debouncing, and keypad key press event calling functionality
; as appropriate. Debouncing is blocking; we do not scan while we debounce.
; The same holds for repeated key-press event calling. 
;
; It's easiest to demonstrate with an example:
;
; Currently, single key presses on any row are supported. Multi-key presses
; will not be reported, but they will be debounced. 
;
; So, if the user presses two keys on row0 of the keypad, but one key on
; row 1, and while scanning we hit row0 first, we'll debounce row0 indefinitely
; and never see the reportable key press on row1 because we stop scanning.
;
; Single key presses on any row held for >= DBP ms timer interrupts will
; result in this function calling a key press. Presses held longer will
; result in first a slow repeated key press being called, for every 
; SRP further ms the key is held. Once this slow repeat event has been
; recognized RPC times, a fast repeated key press will be called
; every FRP ms thereafter.
;
; Operation: 
; Scanning:
;On timer interrupts, we'll scan keypad rows to detect any key presses 
;(high->low). IO input is addressed using a code segment constant array
;of length 2^2 (4 rows) so we can index and wrap indices using an
;11B AND mask. To get the low four bits of this input, we AND against
;the mask UNPRESSED_COLUMN_VALUES, and anything not matching
;UNPRESSED_COLUMN_VALUES (in the low 4 bits) is considered a key press.
;This changes our state to Debouncing after setting PREVIOUS_PRESS_STATE,
;so we know what we're trying to debounce relative to.

; Debouncing:
; If one or more buttons on a row have been pressed, we'll stop scanning
;and start debouncing the presses for that row against the given
;PREVIOUS_PRESS_STATE.

;Staying on that row, we'll sample for up to DBP ms. If the pattern stays
;constant, we know the bouncing has stopped and we can treat it as a real
;button press. There are 16 such patterns possible for any given row in a 4x4 
;keypad. That is, we're allowing multi-key presses at this stage, but only on
;the same row - pressing multiple rows simultaneously will yield non-determin-
;istic results from the user's point of view because whatever is scanned to
;first will be debounced.

;If the pattern changes anytime in the DBP ms, we immediately switch back
;to scanning mode, moving to the next row. If the key press is real, it'll 
;stabilize sometime, and we'll catch it then.

;If the pattern debounces, and is not a supported key press, listed in
;keyPressValuesArray1D, we ignore it. 
;Otherwise, for any of the supported presses, we have a key press event, so
;EnqueueEvent must be called with the appropriate keypress event/value pair.
;For this we call CallAKeyPress to set up the appropriate registers and
;key-press event/value pairs.

;We now switch to the slow-repeat rate mode to keep counting key presses 
;at some frequency as long as the key stays pressed.

;Slow Repeat Rate/Fast Repeat Rate:
; So long as the pattern in our PREVIOUS_PRESS_STATE stays constant,
;at the interrupt-divided frequency dictated by the constants SRP/FRP, we
;call new key presses.

; If the pattern ever changes, we switch back to scanning, on the next row.
; If we call more than RPC rate presses in SlowRPT mode, we switch to 
; FastRPT mode.

; Arguments: none
; Return Values: none
; Local Variables (with persistent meaning):
; BX 	counter intermediate storage
; AL    masked low 4 bits of the keypad IO input for the current row
; CL 	state bit test byte
;
; Constants: 
; SRP, FRP, DBP, RPC (counter frequencies)
; STATE_SCANNING, STATE_DEBOUNCING, STATE_SLOWREPEAT, STATE_FASTREPEAT
; as state bits
; UNPRESSED_COLUMN_VALUES as the unpressed keypad base state
; rowAddressArray1D as the row index to row address map
;
; Shared Variables: 
; CURRENT_ROW 		 		0..numRows-1 index for rowAddressArray1D
; INTERRUPT_COUNT 			current count appropriate to the Process_State
; CURRENT_PROCESS_STATE     Scanning, Debouncing, SlowRPT, FastRPT
; PREVIOUS_PRESS_STATE      Previously pressed keys 
; VARIABLE_RATE_COUNTER     cycles left till switch to FastRPT when SlowRPTing
;
; Global Variables: none
; Input: U15A DB0-3 inputs addressed as 80-83H in IO space on the target board
; schematic from 30-Jan-2008. That is, for a given IO call in this space,
; we're reading the column voltages from the keypad array for the given row
; selected (address => output of U14A chip goes active low, allowing
; key presses to drive column voltages low).
; Output: none
; Error Handling: none
; Registers Used: AX, BX, CX, DX, SI
; Stack Depth: 8 words, flags
; Algorithms: count and array index (0..2^k - 1) wrapping via AND masking
; Data Structures: simple arrays
; Known Bugs: none
; Limitations: Multikey presses on separate rows will have non-deterministic
; results (from the user's point of view) as scanning stops on debouncing.
; Revision History:
; 	12/17/14 SSundaresh created

; Pseudocode
; It's easiest to think about this as state transitions via current state and
; keypress IO inputs. Variables not mentioned in a transition are unchanged.
; State transitions are implemented by changing
; CURRENT_PROCESS_STATE appropriately.

; The actual code is a streamlined version of the code below.

; State 			Input (Condition 1)
;  					UNPRESSED_COLUMN_VALUES 	; no keys pressed		
; Scan(CURRENT_ROW) Scan(CURRENT_ROW+1)         ; keep scanning
; DB 				Scan(CURRENT_ROW+1)      
; SlowRPT   		Scan(CURRENT_ROW+1)      
; FastRPT   		Scan(CURRENT_ROW+1)      

; State 			Input (Condition 2)
;  					PRESS_STATE masked by UNPRESSED_COLUMN_VALUES ; on this row
;						is not UNPRESSED_COLUMN_VALUES  ; a key or more pressed
; Scan(CURRENT_ROW) DB(	PREVIOUS_PRESS_STATE = masked PRESS_STATE, 
;					 	INTERRUPT_COUNT = DeBouncePeriod)    ; start debounce
; DB 			 	n/a                 
; SlowRPT    		n/a
; FastRPT   		n/a	

; State 			Input (Condition 3)
;  					PREVIOUS_PRESS_STATE masked by UNPRESSED_COLUMN_VALUES 
														; key press stable
; Scan(CURRENT_ROW) n/a                             

;					if INTERRUPT_COUNT > 0 
; DB 					DB(INTERRUPT_COUNT -= 1)
;					else
; 						CallAKeyPress
;						SlowRPT(INTERRUPT_COUNT = SRP,
;								VARIABLE_RATE_COUNTER = RPC)								

; SlowRPT   		if INTERRUPT_COUNT > 0
;						SlowRPT(INTERRUPT_COUNT -= 1)
;					else
;						CallAKeyPress
;						VARIABLE_RATE_COUNTER -= 1
;						if VARIABLE_RATE_COUNTER <= 0
;							FastRPT(INTERRUPT_COUNT = FRP)
;						else 
;							SlowRPT(INTERRUPT_COUNT = SRP)					

; FastRPT    		if INTERRUPT_COUNT > 0
; 						FastRPT(INTERRUPT_COUNT -= 1)
; 					else
; 						CallAKeyPress
; 						FastRPT(INTERRUPT_COUNT = FRP)


; State 			Input (Condition 4)
;  					not PREVIOUS_PRESS_STATE masked by UNPRESSED_COLUMN_VALUES
 														; key press unstable 													
; Scan(CURRENT_ROW) n/a 								; return to scanning 
; DB 				Scan(CURRENT_ROW + 1)
; SlowRPT 			Scan(CURRENT_ROW + 1)
; FastRPT   		Scan(CURRENT_ROW + 1)

KeypadEH PROC NEAR
         PUBLIC KeypadEH
	PUSHA 								;save registers

										; Read keypad input

	MOV AL, CURRENT_ROW 				;get current row index 0..numRows-1
										;in rowAddressArray1D
	XOR AH, AH 							;AX is now row index    
	ADD AX, offset(rowAddressArray1D) 	;find actual location of row IO address
    MOV SI, AX                          

	MOV BL, CS:[SI] 					;read IO address into BL
    XOR BH, BH 
    MOV DX, BX                          ;IN AL <- DX

	IN AL, DX
	AND AL, UNPRESSED_COLUMN_VALUES 	
										;select only true keypress input bits
	XOR AH, AH 							;clear AH so AX is IO input

										; identify current state

										; ################
	MOV CL, STATE_SCANNING  			
	CMP CL, CURRENT_PROCESS_STATE 		; are we scanning?
	JE SCANNING
	JNE NOT_SCANNING

	SCANNING: 							; AL contains current row input
	CMP AL, UNPRESSED_COLUMN_VALUES 	;equality means no keys pressed
	JNE TRANSITION_TO_DEBOUNCING
	JMP TRANSITION_TO_SCANNING	

	TRANSITION_TO_DEBOUNCING: 			; set state to Debouncing	 										 						
    MOV CL, STATE_DEBOUNCING
    MOV CURRENT_PROCESS_STATE, CL	
										; set previous press state to current
	 									; keypad row input masked into AL
	MOV PREVIOUS_PRESS_STATE, AL
	
	MOV BX, DBP           				; set up for SetKPInterruptCount call
	CALL SetKPInterruptCount 	 		; set Interrupt count to DBP
	JMP KeypadEH_Done 					; state transition to debounce complete

										; ################	

	NOT_SCANNING:
				 						; AL contains current row input
	CMP AL, PREVIOUS_PRESS_STATE 	 	;equality means keys were stable 
	 									;since we last checked
	JNE TRANSITION_TO_SCANNING
										; input stable so far, so 
	Call DEC_InterruptCount  			; decrement Interrupt_Count
										; BX contains new interrupt count
	
	CMP BX, 0        					; is interrupt count 0? 
	JE CALLKEYPRESS
	JG STILLCOUNTING

 										; ################	
	CALLKEYPRESS:
	CALL CallAKeyPress 					; we've confirmed this key press
  	 									; CallAKeyPress will determine
  	 									; whether the press is supported										

 	 									; ok, are we debouncing, 
  	 									; slow-repeating,
  	 									; or fast-repeating?
	
	MOV CL, STATE_DEBOUNCING
	CMP CL, CURRENT_PROCESS_STATE 		; are we debouncing?
	JE DEBOUNCING_COUNT0
	JNE NOT_DEBOUNCING_COUNT0

	DEBOUNCING_COUNT0:
 	 	
  	 									; now, transition to SlowRepeat mode.
    MOV CL, STATE_SLOWREPEAT
	MOV CURRENT_PROCESS_STATE, CL               ; update state 
	MOV BX, SRP           						; update InterruptCount for
	 											; SlowRPT rate
	CALL SetKPInterruptCount 
	MOV VARIABLE_RATE_COUNTER, RPC      		; prepare new counter for 
	  											; fast-repeat mode
	  											; transition
 	JMP KeypadEH_Done

 	NOT_DEBOUNCING_COUNT0:
 										; are we slow-rpting or fast-rpting?
 	MOV CL, STATE_SLOWREPEAT
	CMP CL, CURRENT_PROCESS_STATE 		; we slowly repeating presses?
	JE SLOW_REPEAT_COUNT0
	JNE FAST_REPEAT_COUNT0

	SLOW_REPEAT_COUNT0:

	DEC VARIABLE_RATE_COUNTER 			; one more slow-repeat cycle done, 
	 									; so one less cycle till we go to 
	 									; fast repeat mode.
	CMP VARIABLE_RATE_COUNTER, 0
	JE TRANSITION_TO_FAST_REPEAT
	JG STILL_SLOW_REPEATING

	TRANSITION_TO_FAST_REPEAT:
    MOV CL, STATE_FASTREPEAT
	MOV CURRENT_PROCESS_STATE, CL                         	; update state 
	MOV BX, FRP          									; update counter
	CALL SetKPInterruptCount
	JMP KeypadEH_Done 										; done

	STILL_SLOW_REPEATING:
	MOV BX, SRP
	CALL SetKPInterruptCount  			; reset interrupt count
	JMP KeypadEH_Done 					; dont change state, so done.
	
	FAST_REPEAT_COUNT0:
	MOV BX, FRP
	Call SetKPInterruptCount 			; reset interrupt count
	JMP KeypadEH_Done 				 	; don't change state, so done.

 										; ################	
  	STILLCOUNTING:
  										; whether we are debouncing, 
  	 									; slow-repeating,
  	 									; or fast-repeating,
  	 									; we've already decremented the
  	 									; count, and we don't want to change 
  	 									; state, so .. do nothing.
  	JMP KeypadEH_Done

	
	TRANSITION_TO_SCANNING:
    MOV CL, STATE_SCANNING
	MOV CURRENT_PROCESS_STATE, CL                   ; set state to Scanning		
	
	MOV AL, CURRENT_ROW 							; increment row
	INC AL

	MOV BL, numRows
	DEC BL
	AND AL, BL
	MOV CURRENT_ROW, AL 				 			; mod row by 
	 												; numRows (2^k-1 mask)
	JMP KeypadEH_Done

	KeypadEH_Done: 						;transitions finalized
	POPA 								;restore registers
	RET
KeypadEH ENDP

; Functional Spec: SetKPInterruptCount
; Description: Set INTERRUPT_COUNT to value in BX
; Operation: Set INTERRUPT_COUNT variable first byte to the low of BX, and
; its second byte to the high byte of BX. SI is mutated.
; Arguments: BX contains the integer word count to set
; Return Values: none
; Local Variables: BX as the argument; SI as the INTERRUPT_COUNT offset in DS
; Constants: none
; Shared Variables: INTERRUPT_COUNT
; Global Variables: none
; Input: none
; Output: none
; Error Handling: none
; Registers Used: SI, BX
; Stack Depth: 0 words
; Algorithms: none
; Data Structures: none
; Known Bugs: none
; Limitations: none
; Revision History:
; 	12/17/14 SSundaresh created
SetKPInterruptCount PROC NEAR
 				    PUBLIC SetKPInterruptCount
 	MOV SI, offset(INTERRUPT_COUNT)	
	MOV DS:[SI], BL 					; set counter to BX
										; low byte, high byte ordering
	INC SI
	MOV DS:[SI], BH
	RET
SetKPInterruptCount ENDP 


; Functional Spec: DEC_InterruptCount
; Description: Decrement INTERRUPT_COUNT by 1. Returns decremented
; INTERRUPT_COUNT in BX.
; Operation: Move INTERRUPT_COUNT variable first byte to the low of BX, and
; its second byte to the high byte of BX. Decrement BX. Mutates SI, BX.
; Arguments: none
; Return Values: BX, decremented Interrupt_Count
; Local Variables:  BX as the decremented interrupt_count.
; 					SI as the INTERRUPT_COUNT offset in DS
; Constants: none
; Shared Variables: INTERRUPT_COUNT
; Global Variables: none
; Input: none
; Output: none
; Error Handling: none
; Registers Used: SI, BX
; Stack Depth: 0 words
; Algorithms: none
; Data Structures: none
; Known Bugs: none
; Limitations: none
; Revision History:
; 	12/17/14 SSundaresh created
DEC_InterruptCount PROC NEAR
 				    PUBLIC DEC_InterruptCount
 	MOV SI, offset(INTERRUPT_COUNT)		
										; low byte, high byte ordering
										; from SetKPInterruptCount
	MOV BL, DS:[SI] 
	INC SI
	MOV BH, DS:[SI]
	
	DEC BX  							; INTERRUPT_COUNT -= 1

	MOV DS:[SI], BH
	DEC SI      						; low byte, high byte ordering
	MOV DS:[SI], BL  					; as before

	RET
DEC_InterruptCount ENDP 

; Functional Spec: CallAKeyPress
; Description: Find the PREVIOUS_PRESS_STATE that was stable enough
; to be called as a key press event in the keyPressValueArray1D. 
; If you find it, it's a supported press, so call EnqueueEvent with
; the appropriate key event and key value, defined in keyPressValueArray1D
; and rowValuesArray1D and EVENT_KEYPRESS. Otherwise do nothing.

; Operation: Save all registers to be safe, since I don't yet know what
; EnqueueEvent calls. Search through entire keyPressValueArray1D for
; a match to PREVIOUS_PRESS_STATE - if found before we hit the end of the
; value array, we have a supported press and we should call EnqueueEvent.
; If not, we just return.

; If there is a supported event, we place the key event identifier in AH
; and the found key value in AL, then add the row value to AL,
; then simply call EnqueueEvent with its argument in AX, then return
; after restoring the registers.

; Arguments: PREVIOUS_PRESS_STATE that was called as a key press event,
; to be matched to a key press event/value pair, if supported.

; Return Values: none
; Local Variables:  BX as the keyValue array absolute index
; 					CX as the keyValue array relative index
; 					AX as the value to pass EnqueueEvent
; Constants: 
; 	keyPressValuesArray1D contains supported keyPress masked IO input values
; 	rowValuesArray1D     contains row keypress event value mapping
; 	EVENT_KEYPRESS 		 keypress event identifier
; 	numSupportedKeyPresses length of keyPressValueArray1D array
;
; Shared Variables: PREVIOUS_PRESS_STATE as the key press to enqueue
; Global Variables: none
; Input: none
; Output: none
; Error Handling: none
; Registers Used: AX, BX
; Stack Depth: 8 words, flags
; Algorithms: none
; Data Structures: 1d array holding event/value map
; Known Bugs: none
; Limitations: none
; Revision History:
; 	12/17/14 SSundaresh created

; PseudoCode
; 						save all registers
; 						try to find PREVIOUS_PRESS_STATE in 
;							keyPressValuesArray1D
; 						if you can,						
; 						EnqueueEvent(
;							EVENT_KEYPRESS,
;							PREVIOUS_PRESS_STATE value in keyPressValueArray1D
;								+ rowValuesArray1D[CURRENT_ROW]
;						)
; 						else, if you can't,
;							do nothing. unsupported key press.

CallAKeyPress PROC NEAR
 			  PUBLIC CallAKeyPress
 	PUSHA 								 ; save registers  	
 	MOV BX, offset(keyPressValuesArray1D) ; find start CS:keyPressValueArray1D
 	MOV CX, 0 							 ; relative index from 0
 	keyPressSupportedLoopStart:
 	CMP CX, numSupportedKeyPresses 		 
 	JE CallAKeyPressDone				 ; we reached the end of the 
 	 									 ; value array without finding our
 	 									 ; PREVIOUS_PRESS_STATE - it isn't
 	 									 ; supported.	
 	MOV AL, CS:[BX]
 	CMP AL, PREVIOUS_PRESS_STATE 		 ; get element from value array
 	 									 ; compare to our PREVIOUS_PRESS_STATE
 	 									 ; a match indicates a supported press
 	JE keyPressSupported

 	INC CX 								 ; to next element in our value array
 	INC BX
 	JMP keyPressSupportedLoopStart 	

 	keyPressSupported: 					; AL already contains keypress value
 	MOV BL, CURRENT_ROW  				; locate row index
 	XOR BH, BH 							; clear high byte
 	ADD BX, offset(rowValuesArray1D)   	; locate rowValuesArray element
	MOV BL, CS:[BX] 	 				; get row value
 	ADD AL, BL 							; add row value to key value
 	MOV AH, EVENT_KEYPRESS 				; AH contains keypress event id
 	Call EnqueueEvent 					; argument in AX

 	CallAKeyPressDone:
 	POPA  								 ; restore registers
 	RET
CallAKeyPress ENDP

 
CODE ENDS

DATA SEGMENT PUBLIC 'DATA'

; all initialized by KPInit

CURRENT_ROW DB ?  		  ; integer from 0-3, index rowAddressArray1D
 							  
PREVIOUS_PRESS_STATE DB ?   ; previously sampled pressed keys

CURRENT_PROCESS_STATE DB ?  ; hold current state bits

INTERRUPT_COUNT DW ? 		  ; count ms timer interrupts for debouncing, 
							  ; repeat windowed key-press calling
							  ; this will be stored low/high byte, and read out
							  ; and updated equivalently.

VARIABLE_RATE_COUNTER DB ?  ; count number of repeated key-press events at
							  ; the slow rate,
 							  ; to identify when we should increase the 
 							  ; rate of key-press event calling


DATA ENDS

END