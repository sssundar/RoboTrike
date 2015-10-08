NAME Handlers
; Contains
; EnqueueEvent 		buffer serial input, error events
; ProcessEvents 	process EventBuffer, parsing and processing parse errors
; 					respond to catastrophic buffer overflow errors
; ReportError 		Tx error outputs to UI board.
; TransmitStatus 	Tx wrapper to send motor angle status to UI board
; ClearEOFlag, SetEOFlag, and GetEOFlag helper functions for error flag access
; Handlers_EventQueue_Init initializes the handler shared variables.
; Last Revision 12/29/2014 SSundaresh created

; fixedLengthQueue structure, and the 
; QUEUE_SIZE globally applied to all event queues.
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (PIOConst.inc)
$INCLUDE (queues.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

EXTRN QueueInit:NEAR
EXTRN QueueEmpty:NEAR, QueueFull:NEAR
EXTRN Dequeue:NEAR, Enqueue:NEAR

EXTRN SerialPutChar:NEAR, LoopTillSerialPut:NEAR 
EXTRN ResetParserState:NEAR, ParseSerialChar:NEAR

EXTRN SetMotorSpeed:NEAR
EXTRN GetMotorSpeed:NEAR
EXTRN GetMotorDirection:NEAR
EXTRN Dec2String:NEAR
EXTRN PIOChipSel:NEAR
EXTRN Disable_PWM_Timer1:NEAR

EXTRN Motor_Error_String:BYTE

;EnqueueEvent (called only from SerialIO (Motor) or SerialIO/KP (UI)
;				 interrupt handlers)
;	Takes as input in AH:AL an event ID: value. 
;
;	IDs might be.. Serial Error, SerialRx, KP (on UI boarD)/
;	not ParseError, EventBuf Full Error
;	as these are handled outside EnqueueEvent.	
;	
;	Checks if an event buffer is full. If yes:
;		On Motor Board: ignores the passed argument.
;						since serial IO on the motor board can be
;						interrupted by PWM timers, opens a CLI/STI block.
;							disables timer interrupts
;							sets parallel output bits to 0
;						exits cli/sti block
;						sets CE flag.
;						returns.
;					no matter what instruction processevents loop was
;					handling, only the pwm timers control PIO bits.
;					so motors off indefinitely now. 
;					now ProcessEvents loop 
;					will recognize CE flag on next cycle
;					and start transmitting appropriate error message 
;					never stops.
;					only recovery is a power cycle.	
;	If no, 
;		Enqueue word (ID:value) argument (not critical, only function
;			touching this queue)
;		return
; Limitations:   	only called by interrupts that can't nest. not critical.
; Revision History:
;   12/29/2014 SSundaresh  created
; Pseudocode
EnqueueEvent PROC NEAR 
                PUBLIC EnqueueEvent
    PUSH AX
    MOV SI, offset(EventBuffer)
    CALL QueueFull      ; doesn't modify SI
    JNZ DROP_AND_DASH
    JZ DOGASTROPHIC_ERROR

    DOGASTROPHIC_ERROR:
    POP AX				; EventBuffer overflow on MotorBoard
    CLI 				; catastrophic error (from my POV)
    CALL Disable_PWM_Timer1 ; disable PWM interrupts
    CALL PIOChipSel 	; Reset Motor PIO controller
     					; now no call can restart motors     				
    STI
    MOV BX, offset(Catastrophic_Error)
    MOV AL, 1
    MOV DS:[BX], AL 		; set CE flag active
    JMP EnqueueEvent_DONE

    DROP_AND_DASH:
    POP AX 				; SI address, AX value    
    CALL Enqueue
    ;JMP EnqueueEvent_DONE

    EnqueueEvent_DONE:
    RET
EnqueueEvent ENDP

; Helper Function
ClearEOFlag PROC NEAR
			PUBLIC CLearEOFlag
	PUSH BX
	PUSH AX
	MOV BX, offset(Error_Occurred)
	MOV AL, 0
	MOV DS:[BX], AL
	POP AX
	POP BX
	RET
ClearEOFlag ENDP

; Helper Function
; Mutates BX, returns EOF in BL
GetEOFlag PROC NEAR
 			PUBLIC GetEOFlag	
 	PUSH AX
	MOV BX, offset(Error_Occurred)	
	MOV AL, DS:[BX]		
	MOV BL, AL
	POP AX
	RET 			
GetEOFlag ENDP

; Helper Function
SetEOFlag PROC NEAR
			PUBLIC SetEOFlag
	PUSH BX
	PUSH AX
	MOV BX, offset(Error_Occurred)
	MOV AL, 1
	MOV DS:[BX], AL
	POP AX
	POP BX
	RET
SetEOFlag ENDP

;ProcessEvents (Motor_ML will call this repeatedly)
;	(Only function that can reset Error_Occurred Flag)
;	check CE flag.
;		if high, Transmit CE message loop
;		otherwise,	
;			Dequeue the next event from the EventBuffer.
;
;			Parse the value specially -
; 				whether or not there is an EO flag, 
;					If the event type is an error, report it by calling 
;					ReportError. 	
;					Otherwise, 
;					the input is a character-type, so parse away and execute 
;					if possible. If the EOFlag is set, it won't execute
; 					anything but the command R to reset the error state,
; 					and we don't need to check the parse error flag returned.
; 					If the EOFlag is not set, all commands are fair game, 
;					if there is a parser error returned by parseserialchar,
;					report this error.
ProcessEvents PROC NEAR 
                PUBLIC ProcessEvents
    MOV BX, offset(Catastrophic_Error)
    MOV AL, DS:[BX]
    OR AL, AL 	 	; is CE flag set?
    JNZ CE_SET
    JZ CE_NOT_SET

    CE_SET:
    MOV AL, EBF_ERROR
    CALL ReportError  	; might fill Tx Queue but just keep sending
     					; nothing else to do.
    JMP PE_DONE

    CE_NOT_SET:
    MOV SI, offset(EventBuffer)
    CALL QueueEmpty 		
    JZ PE_DONE
    JNZ Process_Event

    Process_Event:
    Call Dequeue 				 			; event ID in AH, value in AL    										
    MOV BH, SERIAL_ERROR_EVENT_ID
    CMP BH, AH  						; is our event a serial error?
    JE REPORT_SerialError
    MOV BH, SERIAL_CHAR_EVENT_ID 		; is our event a character?
    CMP BH, AH
    JE PE_ParseChar
    SIT_HERE:
    JNE SIT_HERE	 					; should be only two options

    REPORT_SerialError:
    CALL ReportError 					; argument in AL
    JMP PE_DONE

    PE_ParseChar:
    CALL ParseSerialChar 				; argument is in AL	, returns in AX
	Call GetEOFlag 						; returns in BL
	OR BL, BL	
	JNZ PE_DONE 						; if in EO state don't care about
										; parser return value 
	JZ CHECK_4ParseError

	CHECK_4ParseError:    
	OR AX, AX 			; if 0, no parsing errors, else parsing errors.
	JZ PE_DONE
	MOV AL, GPE_ERROR 	; report general parsing error
	Call ReportError
    ;JMP PE_DONE

    PE_DONE:
    RET
ProcessEvents ENDP

; Functional Spec, ReportError
; Description: This function will only be called from non-interrupt code.
; Takes an error byte value in AL, containing an ASCII 1-9
; to be transmitted to the UI board. The grammar for the transmission is 
; \CR R (AL) \CR with no whitespace. Adds this error message to the
; Tx queue using SerialPutChar. 
; Sets Error_Occurred Flag to 1, active, so we know to look for an R command
; resetting the error, in ProcessEvents. 
; Operation: Reads error format from code-string in Errors.asm. Looks for
; null character and inserts AL there instead. Transmits sequentially.
; Sets error flags, resets parser state, halts motors.
; Arguments: AL, ASCII 1-9      
; Return values:    none
; Constants:  		ASCII_CR, Motor_Error_String, Motor_Error_String_Len
; 					ASCII_NULL
; Local Variables:  
; Shared Variables: none
; Global Variables: none
; Registers used:   none - pushes, pops
; Stack depth:      8 words
; Input:       		none
; Output:           Calls SerialPutChar for message Tx
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs: 		none
; Limitations:   	do not call from interrupt handlers.
; Revision History:
;   12/29/2014 SSundaresh  created
; Pseudocode
ReportError	PROC NEAR 
            PUBLIC ReportError  
    PUSHA           
    MOV CL, 0 
    MOV CH, Motor_Error_String_Len
    MOV BX, offset(Motor_Error_String)
		
	KEEP_WRITING_TX_BUFFER:
    CMP CL, CH
    JE DONE_REPORTING_ERROR           
    PUSH CX
    PUSH BX
    PUSH AX
    XOR CH, CH
    ADD BX, CX
    MOV AL, CS:[BX]
    CMP AL, ASCII_NULL
    JE REPLACE_WITH_ERROR_VALUE

	CALL LoopTillSerialPut 				; write the next template character
    JMP POP_AX 					

    REPLACE_WITH_ERROR_VALUE:
    POP AX
    CALL LoopTillSerialPut 				; write the passed error code
    JMP ONWARD_HO

    POP_AX:
	POP AX    
    
    ONWARD_HO:
    POP BX
    POP CX
    INC CL
    JMP KEEP_WRITING_TX_BUFFER   

    DONE_REPORTING_ERROR:
    CALL SetEOFlag 				; set error occurred flag for ProcessEvents
    CALL ResetParserState 		; reset parser till we see R command
    MOV AX, 0
    MOV BX, 0
    CALL SetMotorSpeed 			; halt

    POPA
    RET
ReportError ENDP

; TransmitStatus
; Description: Preserves registers. Transmits
; motor current direction from [0-359] with proper
; formatting for Motor-to-UI serial IO, specified
; as \CR D +00### \CR without whitespace, with ###
; as the direction. 
; Last Revised: 12/29/2014 SSundaresh created
TransmitStatus PROC NEAR
			PUBLIC TransmitStatus
	PUSHA
	CALL GetMotorDirection 		; AX has direction
	MOV SI, offset(StatusString) 	; StatusString is in DS
	CALL Dec2String 		; DS:StatusString has +00###ASCII_NULL

	MOV AL, ASCII_CR 		; transmit status header
	CALL LoopTillSerialPut
	MOV AL, 'D'
	CALL LoopTillSerialPut 	
	
	MOV SI, offset(StatusString)
	TX_ANGLE_STATUS:
	MOV AL, DS:[SI]
	CMP AL, ASCII_NULL
	JE TX_TERMINATION_CHAR
	PUSH SI
	CALL LoopTillSerialPut
	POP SI		
	INC SI
	JMP TX_ANGLE_STATUS

	TX_TERMINATION_CHAR:
	MOV AL, ASCII_CR 		; transmit status termination character
	CALL LoopTillSerialPut

	POPA
	RET
TransmitStatus ENDP

; Functional Spec: Handlers_EventQueue_Init
; Description:      Initializes EventBuffer as a word queue, clears error flags
; Operation:        n/a
; Shared Variables: EventBuffer
; Constants:        WORD_QUEUE_LEN
; Registers used:   SI, AX, BL, see Queues\QueueInit
; Revision History:
;   12/29/2014 SSundaresh  created
Handlers_EventQueue_Init   PROC NEAR 
                PUBLIC Handlers_EventQueue_Init
    CALL ClearEOFlag 					; clear EO flag
    MOV BX, offset(Catastrophic_Error) 	; clear CE flag
    MOV AL, 0
    MOV DS:[BX], AL

    MOV SI, offset(EventBuffer)         ; SI holds queue address as DS:SI
    MOV AX, WORD_QUEUE_LEN
    					          	; length is WORD_QUEUE_LEN
    MOV BL, 1                       ; element size words
    CALL QueueInit
    RET
Handlers_EventQueue_Init ENDP

CODE ENDS

;Shared Variables
DATA    SEGMENT PUBLIC  'DATA'

;(EO) boolean byte (0 no,1 yes)
Error_Occurred DB ?
;(CE) boolean byte (0 no, 1 yes)
Catastrophic_Error DB ?

;  our EventBuffer word queue
;  maximum elements, QUEUE_SIZE-1
;  see SerialIO.inc
EventBuffer fixedLengthQueue  < >

; holds binary-to-decimal converted GetMotorDirection status
; only length 7 to match Dec2String spec - sign, 5 digits, ASCII_NULL
StatusString DB 7 DUP (?)

DATA    ENDS

END