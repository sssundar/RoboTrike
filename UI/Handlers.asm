NAME Handlers
; Contains
; EnqueueEvent 		buffer serial input, serial error, and keypad input events
; ProcessEvents 	process EventBuffer, parsing and processing parse errors
; 					respond to catastrophic buffer overflow errors

; ReportError 		Display error messages at CS:SI, set error flag, 
;                   reset parser state.
; ClearEOFlag, SetEOFlag, and GetEOFlag helper functions for error flag access

; and functions for readying UI board state for executing keypad commands:
; TxKeypadCommand(keypad command identifier)
    ; SetupForLeftwardMovement
    ; SetupForForwardMovement
    ; SetupForRightwardMovement
    ; SetupForBackwardMovement
    ; SetupForLeftwardTurn
    ; SetupForRightwardTurn
    ; SetupForAbsoluteReset
    ; SetupForLaserOn
    ; SetupForLaserOff
    ; SetupForVelocityInc
    ; SetupForVelocityDec

    ; which use helper functions
    ; LoopTillStringPut (SerialIO)
    ; SetupForMovementRelativeToReference
    ; SetupFBLRCommandStringsForTx
    ; AdditionMod360

    ; which in turn use helper functions
    ; UIAwaitingStatus
    ; UIIntentConsistentWithReportedStatus 
    ; UpdateUIAngleStatus        
    ; and 
    ; SetAwaitingAngleStatusFlag
    ; ClearAwaitingAngleStatusFlag, UIAwaitingStatus (getter)
    ; SetReferenceDeltaIntended, GetReferenceDeltaIntended
    ; SetCurrentDirectionIntended, GetCurrentDirectionIntended
    ; SetReference, GetReference
    ; SetCurrentDirection, GetCurrentDirection
    ; SetWordAtOffset, GetWordAtOffset

; Handlers_EventQueue_Init initializes the handler shared variables.

; The core premise on which these functions are built: it is very unlikely
; that we transmit a command, and do not receive the status (error or not) 
; in full, well before we even see another keypress event. This is
; just because of how the timing is setup for debouncing (10Hz) relative to
; the timescale of serialIO and the entire motor board operation cycle,
; which takes ~90 ms in the worst case, even with errors.

; Last Revision 12/29/2014 SSundaresh created for motors board
;               12/31/2014 SSundaresh updated for UI board

; fixedLengthQueue structure, and the 
; QUEUE_SIZE globally applied to all event queues.
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (queues.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

EXTRN QueueInit:NEAR
EXTRN QueueEmpty:NEAR, QueueFull:NEAR
EXTRN Dequeue:NEAR, Enqueue:NEAR

EXTRN SerialPutChar:NEAR, LoopTillSerialPut:NEAR 
EXTRN ResetParserState:NEAR, LoopTillStringPut:NEAR
EXTRN ParseSerialChar:NEAR

EXTRN SetZF:NEAR, ClearZF:NEAR
EXTRN Dec2String:NEAR
EXTRN Display:NEAR

EXTRN UI_Overflow_Error_String:BYTE
EXTRN UI_Serial_Error_String:BYTE
EXTRN UI_Parse_Error_String:BYTE

EXTRN Tx_LaserOn_String:BYTE
EXTRN Tx_LaserOff_String:BYTE
EXTRN Tx_AngleIncrease_String:BYTE
EXTRN Tx_AngleDecrease_String:BYTE
EXTRN Tx_SpeedIncrease_String:BYTE
EXTRN Tx_SpeedDecrease_String:BYTE
EXTRN Tx_Reset_String:BYTE
EXTRN Tx_FBLR_Template_String:BYTE
EXTRN KP_Reset_Display_String:BYTE
EXTRN KP_Forward_Display_String:BYTE
EXTRN KP_Backward_Display_String:BYTE
EXTRN KP_Left_Display_String:BYTE
EXTRN KP_Right_Display_String:BYTE
EXTRN KP_LaserOn_Display_String:BYTE
EXTRN KP_LaserOff_Display_String:BYTE
EXTRN KP_AngleIncrease_Display_String:BYTE
EXTRN KP_AngleDecrease_Display_String:BYTE
EXTRN KP_VelocityIncrease_Display_String:BYTE
EXTRN KP_VelocityDecrease_Display_String:BYTE


;EnqueueEvent (called only on SerialIn/keypress event (UI), in 
;       interrupt handlers)
;	Takes as input in AH:AL an event ID: value. 
;
;	IDs might be.. UI-side Serial Error, UI-SerialRx, UI-KP
;	not ParseError, EventBuf Full Error
;	as these are handled outside EnqueueEvent.	
;	
;	Checks if an event buffer is full. If yes:
;		On UI board: ignores the passed argument.
;					    sets CE flag.
;						returns.
;					now ProcessEvents loop 
;					will recognize CE flag on next cycle
;					and start displaying appropriate error message 
;                   and stops processing new events.
;					only recovery is a power cycle.	
;	If no, 
;		Enqueue word (ID:value) argument (not critical, only one function
;			can touch this queue to enqueue at any single time,
;           and only from interrupt handlers, without nesting)
;		return
; Limitations:   	only called by interrupts that can't nest. not critical.
; Revision History:
;   12/29/2014 SSundaresh  created for motors
;   12/31/2014 SSundaresh  updated for UI
EnqueueEvent PROC NEAR 
                PUBLIC EnqueueEvent
    PUSH AX
    MOV SI, offset(EventBuffer)
    CALL QueueFull      ; doesn't modify SI
    JNZ DROP_AND_DASH
    JZ DOGASTROPHIC_ERROR

    DOGASTROPHIC_ERROR:
    POP AX				; EventBuffer overflow on MotorBoard
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
;	(Only function that can reset Error_Occurred Flag, indirectly)
;    if CE
;        if WCEM, just return.
;        else set WCEM and display CE message using reporterror.
;   else
;        dequeue event
;        if error event from serial IO
;              report error from errors.asm UI SIO error messages.
;        if keypad event, call keypad handler. this will ignore all but Reset
;        presses, which reset the EO flag and the parser state.
;        if serial rx ignore if EOFlag set, else parse and report parse errors.
; Note: takes for granted timing set up so new-commands sent with 
; status report still pending does not happen. 
ProcessEvents PROC NEAR 
                PUBLIC ProcessEvents
    MOV BX, offset(Catastrophic_Error)
    MOV AL, DS:[BX]
    OR AL, AL 	 	; is CE flag set?
    JNZ CE_SET
    JZ CE_NOT_SET

    CE_SET:
    MOV BX, offset(Wrote_CE_Message)
    MOV AL, DS:[BX]
    OR AL, AL
    JNZ PE_DONE
    MOV SI, offset(UI_Overflow_Error_String)
    CALL ReportError
    JMP PE_DONE

    CE_NOT_SET:
    MOV SI, offset(EventBuffer)
    CALL QueueEmpty 		
    JZ PE_DONE
    JNZ Process_Event

    Process_Event:
    Call Dequeue 				 			; event ID in AH, value in AL 
    MOV BH, SERIAL_ERROR_EVENT_ID
    CMP BH, AH  						; is our event a UI serial error?
    JE REPORT_SerialError
    MOV BH, EVENT_KEYPRESS              ; is our event a keypress?
    CMP BH, AH
    JE HANDLE_Keypress
    MOV BH, SERIAL_CHAR_EVENT_ID 		; is our event a character?
    CMP BH, AH
    JE PE_ParseChar
    SIT_HERE:
    JNE SIT_HERE	 					; should be only three options

    HANDLE_Keypress:
    CALL TxKeypadCommand
    JMP PE_DONE

    REPORT_SerialError:
    MOV SI, offset(UI_Serial_Error_String)  ; don't care which one, really.
    CALL ReportError
    JMP PE_DONE

    PE_ParseChar:
    CALL ParseSerialChar 				; argument is in AL	, returns in AX
	CALL GetEOFlag 						; returns in BL
	OR BL, BL	
	JNZ PE_DONE 						; if in EO state don't care about
										; parser return value 
	JZ CHECK_4ParseError

	CHECK_4ParseError:    
	OR AX, AX 			; if 0, no parsing errors, else parsing errors.
	JZ PE_DONE
	MOV SI, offset(UI_Parse_Error_String) 	; report general parsing error
	Call ReportError
    ;JMP PE_DONE

    PE_DONE:
    RET
ProcessEvents ENDP

; Functional Spec, ReportError
; Description: This function will only be called from non-interrupt code.
; Takes in CS:SI an error message to display. Calls Display. 
; Sets EO flag. Resets Parser state. Returns. 
; Arguments: SI, CS implied
; Return values:    none
; Local Variables:  
; Shared Variables: none
; Global Variables: none
; Registers used:   flags. pushes, pops all.
; Stack depth:      8 words
; Input:       		none
; Output:           Display 
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs: 		none
; Limitations:   	do not call from interrupt handlers.
; Revision History:
;   12/29/2014 SSundaresh  created for motor board
;   12/31/2014 SSundaresh  updated for ui board
ReportError	PROC NEAR 
            PUBLIC ReportError  
    PUSHA           
    CALL Display            
    CALL SetEOFlag
    CALL ResetParserState
    POPA
    RET
ReportError ENDP

; Functional Spec: Handlers_EventQueue_Init
; Description:      Initializes EventBuffer as a word queue, clears error flags
; Operation:        n/a
; Shared Variables: EventBuffer
; Constants:        WORD_QUEUE_LEN
; Registers used:   SI, AX, BL, see Queues\QueueInit
; Revision History:
;   12/29/2014 SSundaresh  created
;   12/31/2014 SSundaresh  added angle status inits for UI board.
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

    CALL ClearAwaitingAngleStatusFlag
    MOV AX, 0                       ; not waiting for anything,
                                    ; and startup state has angle 0
    CALL SetReference                
    Call SetCurrentDirection
    RET
Handlers_EventQueue_Init ENDP

; ######
; The following are Keypad Command handlers, and associated shared variable
; set, clear, and get functions. 
; ######

; Takes a keypad command identifier in AL. Since we handle all errors (
; UI-serial, UI-parsing, M-Serial/Parsing/CE) via the EventBuffer, 
; in non-interrupt code, except for UI-Catastrophic Error, we know if 
; we are here, there is no error yet that we've seen, so we should
; send this command. If CE occurred on our board, then we'll catch it
; next ProcessEvents cycle.
; Note: if CE flag high, we'll never get here from ProcessEvents.
; if Error_Occurred flag is high, we only look for Reset presses. 
; all others do nothing.
TxKeypadCommand PROC NEAR 
        PUBLIC TxKeypadCommand

    CMP AL, KP_Reset
    JE Pressed_Reset                
    
    CALL GetEOFlag              ; EO flag in BL
    OR BL, BL                   ; if 1, error occurred, look only for Reset
    JZ TKC_KEEP_PROCESSING
    JMP TKC_DONE
    
    TKC_KEEP_PROCESSING:
    CMP AL, KP_Forward
    JE Pressed_Forward
    CMP AL, KP_Backward          
    JE Pressed_Backward
    CMP AL, KP_Left    
    JE Pressed_Left          
    CMP AL, KP_Right             
    JE Pressed_Right
    CMP AL, KP_LaserOn           
    JE Pressed_LaserOn
    CMP AL, KP_LaserOff
    JE Pressed_LaserOff          
    CMP AL, KP_VelocityIncrease  
    JE Pressed_VINC
    CMP AL, KP_VelocityDecrease  
    JE Pressed_VDec
    CMP AL, KP_AngleIncrease     
    JE Pressed_AInc
    CMP AL, KP_AngleDecrease     
    JE Pressed_ADec
    JMP TKC_DO_NOTHING               ; non-mapped keypresses not commands
									; but will show up (many unused single keys)
    
    Pressed_Forward:    
    CALL SetupForForwardMovement    ; BX is set to command offset in DS
                                     ; SI is set to display offset in CS
    JMP PrepPt1

    Pressed_Backward:    
    CALL SetupForBackwardMovement    ; BX is set to command offset in DS
                                     ; SI is set to display offset in CS
    JMP PrepPt1

    Pressed_Left:          
    CALL SetupForLeftwardMovement    ; BX is set to command offset in DS
                                     ; SI is set to display offset in CS
    JMP PrepPt1

    Pressed_Right:
    CALL SetupForRightwardMovement    ; BX is set to command offset in DS
                                     ; SI is set to display offset in CS
    JMP PrepPt1

    PrepPt1:
    MOV AX, DS
    MOV ES, AX   
    CALL LoopTillStringPut
    MOV AX, CS
    MOV ES, AX    
    JMP TKC_DONE  

    Pressed_Reset:
    CALL SetupForAbsoluteReset    ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_LaserOn:    
    CALL SetupForLaserOn        ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_LaserOff:              
    CALL SetupForLaserOff       ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_VINC:    
    CALL SetupForVelocityInc   ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_VDec:    
    CALL SetupForVelocityDec   ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_AInc:    
    CALL SetupForLeftwardTurn    ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2
    
    Pressed_ADec:
    CALL SetupForRightwardTurn    ; BX is set to command offset in CS
                                  ; SI is set to display offset in CS
    JMP PrepPt2

    PrepPt2:
                        ; ES is already CS, BX contains command string in CS
    CALL LoopTillStringPut
    JMP TKC_DONE    
    		
    TKC_DONE:           ; ES = CS, SI = display string.
    CALL Display
	TKC_DO_NOTHING:
    RET
TxKeypadCommand ENDP
        
; ######
; The following 4 functions handle FBLR commands, 2 are helpers. The others ret 
; in DS:BX the command string to transmit, and in CX:SI the 
; string to display for each command. Otherwise they change only flags.
; Return ES set to CS for Display calls.
; None take arguments, and they handle setting status-waiting flags, etc. 
; themselves.
; ######

; takes RelRef in AX in [0,359]
; clear RefDeltaIntended, get Reference, then add RelRef degrees to move RelRef
; relative to the current reference. Set CurrentDirectionIntended as Ref+RelRef
; check the current direction,
; calculate the difference (Ref+RelRef)-CurrDir mod 360 to know how much
; to add to CurrDir to get to Ref + RelRef. Return this quantity in AX.
; No additions here can overflow. 
SetupForMovementRelativeToReference PROC NEAR
            PUBLIC SetupForMovementRelativeToReference    
    PUSH BX                         ; store used registers
    PUSH CX

    PUSH AX                         ; store RelRef
    CALL SetAwaitingAngleStatusFlag
    
    MOV AX, 0                       ; not trying to change reference
    Call SetReferenceDeltaIntended

    CALL GetReference               ; Ref in AX
    MOV BX, AX                      ; Ref in BX
    POP AX                          ; get argument RelRef back in AX
    CALL AdditionMod360             ; AX <- Ref+RelRef
    CALL SetCurrentDirectionIntended ; CDI = Ref+RelRef

    MOV BX, AX                      ; store Ref+RelRef in BX
    CALL GetCurrentDirection        ; in AX
    MOV CX, AX
    MOV AX, BX
    MOV BX, CX                      ; exchange AX, BX
    NEG BX                          ; negate BX so BX = -CurrDir
    CALL AdditionMod360             ; AX + BX = (Ref+90) - CurrDir, ret in AX
                                    ; this is our desired return value, in AX

    POP CX                          ; restore used registers
    POP BX
    RET
SetupForMovementRelativeToReference ENDP

; RelRef = +90 degrees. changes flags, AX, BX, and SI
SetupForLeftwardMovement PROC NEAR
        PUBLIC SetupForLeftwardMovement    
    MOV AX, 90
    CALL SetupForMovementRelativeToReference    
    MOV SI, offset(FLBRCommandArgument) 
    CALL Dec2String                     ; AX = n, DS:SI = buffer
                                        ; now n converted to ASCII string
                                        ; with null terminator and sign.
    CALL SetupFBLRCommandStringsForTx
                                        ; now DS:BX pointer to command
                                        ; string to transmit
    MOV SI, offset(KP_Left_Display_String)    
    MOV AX, CS
    MOV ES, AX
    RET
SetupForLeftwardMovement ENDP

; RelRef = +270 degrees. changes flags, AX, BX, and SI
SetupForRightwardMovement PROC NEAR
        PUBLIC SetupForRightwardMovement    
    MOV AX, 270
    CALL SetupForMovementRelativeToReference    
    MOV SI, offset(FLBRCommandArgument) 
    CALL Dec2String                     ; AX = n, DS:SI = buffer
                                        ; now n converted to ASCII string
                                        ; with null terminator and sign.
    CALL SetupFBLRCommandStringsForTx
                                        ; now DS:BX pointer to command
                                        ; string to transmit
    MOV SI, offset(KP_Right_Display_String)        
    MOV AX, CS
    MOV ES, AX
    RET
SetupForRightwardMovement ENDP

; RelRef = +0 degrees. changes flags, AX, BX, and SI
SetupForForwardMovement PROC NEAR
        PUBLIC SetupForForwardMovement    
    MOV AX, 0
    CALL SetupForMovementRelativeToReference   
    MOV SI, offset(FLBRCommandArgument) 
    CALL Dec2String                     ; AX = n, DS:SI = buffer
                                        ; now n converted to ASCII string
                                        ; with null terminator and sign.
    CALL SetupFBLRCommandStringsForTx
                                        ; now DS:BX pointer to command
                                        ; string to transmit
    MOV SI, offset(KP_Forward_Display_String)      
    MOV AX, CS
    MOV ES, AX
    RET
SetupForForwardMovement ENDP

; RelRef = +180 degrees. changes flags, AX, BX, and SI
SetupForBackwardMovement PROC NEAR
        PUBLIC SetupForBackwardMovement    
    MOV AX, 180
    CALL SetupForMovementRelativeToReference    
    MOV SI, offset(FLBRCommandArgument) 
    CALL Dec2String                     ; AX = n, DS:SI = buffer
                                        ; now n converted to ASCII string
                                        ; with null terminator and sign.
    CALL SetupFBLRCommandStringsForTx
                                        ; now DS:BX pointer to command
                                        ; string to transmit
    MOV SI, offset(KP_Backward_Display_String)  
    MOV AX, CS
    MOV ES, AX
    RET
SetupForBackwardMovement ENDP

; a helper functions to format the command string D (argD) at
; DS:offset(FBLRCommandString), for the above.
; Considers DS:SI the argument string for the template command at 
; CS:offset(Tx_FBLR_Template_String), with leading 0s and sign, fitting
; in exactly '+00000' spaces. Copies this CS string 
; to FBLRCommandString, but replaces '0's with values from FLBRComArg.
; There's exactly enough room. 
; Returns in BX the offset to DS:offset(FBLRCommandString).
; Changes registers BX and flags
SetupFBLRCommandStringsForTx PROC NEAR
            PUBLIC SetupFBLRCommandStringsForTx         
    PUSH DI
    PUSH SI
    PUSH DX

    MOV BX, offset(Tx_FBLR_Template_String)     ; command template
    MOV DI, offset(FBLRCommandString)           ; building command
    MOV SI, offset(FLBRCommandArgument)         ; command argument

    SFCSFT_Loop_Start:
    MOV DL, CS:[BX]
    CMP DL, ASCII_NULL      ; terminate
    JE SFCSFT_Loop_Terminate
    CMP DL, '0'             ; start copying argument
    JE REPLACE_VALUE        
    JNE COPY_VALUE

    REPLACE_VALUE:
    MOV DL, DS:[SI]
    MOV DS:[DI], DL
    INC SI
    JMP Index_Updates

    COPY_VALUE:
    MOV DS:[DI], DL
    ;JMP Index_Updates
    
    Index_Updates:
    INC DI
    INC BX
    JMP SFCSFT_Loop_Start
    
    SFCSFT_Loop_Terminate:
    MOV DS:[DI], DL             ; terminate with ASCII_NULL
    POP DX
    POP SI
    POP DI
    MOV BX, offset(FBLRCommandString)
    RET
SetupFBLRCommandStringsForTx ENDP

; ######
; The following 7 functions call D, D, R, F, O, V, and V
; respectively, and do not take variable arguments. 
; They return the CS offset containing
; the unique command string associated with each of R, F, O, V, and V, in BX,
; and the command display string in SI. ES is set to CS as these are all
; constant strings.
; R sets awaiting status, but the rest clear it, and hence do not have
; to clear any more status variables, as the next function to set
; AwaitingStatus will have its own agenda and change the vars itself.
; ######

SetupForLeftwardTurn PROC NEAR
            PUBLIC SetupForLeftwardTurn        
    CALL SetAwaitingAngleStatusFlag
    
    MOV AX, LeftwardTurnAngle
    Call SetReferenceDeltaIntended
    MOV BX, AX
    
    CALL GetCurrentDirection        ; in AX. RDI in BX
    CALL AdditionMod360             ; calculate AX + BX mod 360 -> CDIntended
                                    ; addition yields AX
    CALL SetCurrentDirectionIntended   ; set CDI

    MOV AX, CS
    MOV ES, AX
    MOV BX, offset(Tx_AngleIncrease_String)
    MOV SI, offset(KP_AngleIncrease_Display_String)
    RET
SetupForLeftwardTurn ENDP

SetupForRightwardTurn PROC NEAR
            PUBLIC SetupForRightwardTurn        
    CALL SetAwaitingAngleStatusFlag
    
    MOV AX, RightwardTurnAngle
    Call SetReferenceDeltaIntended
    MOV BX, AX
    
    CALL GetCurrentDirection        ; in AX. RDI in BX
    CALL AdditionMod360             ; calculate AX + BX mod 360 -> CDIntended
                                    ; addition yields AX
    CALL SetCurrentDirectionIntended   ; set CDI
    
    MOV AX, CS
    MOV ES, AX
    MOV BX, offset(Tx_AngleDecrease_String)
    MOV SI, offset(KP_AngleDecrease_Display_String)
    RET
SetupForRightwardTurn ENDP

; Special: we call this under poor circumstances, usually. We don't know
; what the motor state is anymore. So can't follow our usual relative-intended
; angle request, verification process.. suppose the boards are out of sync,
; having missed some commands, or status updates - we'll get a
; neverending error train because intentions stop being based on
; true motor state! 
; So, here, we set AwaitingStatus
; then manually set Reference to 0, and Delta to 0, and CurrDirIntended to 0.
; If the reset works, we'll receive a status update of the form
; \CR D+00000 \CR and our intent checking will say: this matches the
; intended current direction, but internally we also know
; that our reference has been reset.
;
; Implicit in this is the understanding
; that in the absence of UI-motor serial errors, the motor will stay
; synced with us, and that our timing is set up so 
; every command executes are reports its status to the UI board;
; before another command even has time to be enqueued. So we'll never
; be in a state where our intended status is out of sync with our reported
; status, unless a serial error occurred. 
SetupForAbsoluteReset PROC NEAR
            PUBLIC SetupForAbsoluteReset   
    PUSH AX 
    CALL ClearEOFlag                    ; reset clears errors till now.
    CALL ResetParserState               ; shouldn't be necessary, should not
                                        ; hurt either.
    CALL SetAwaitingAngleStatusFlag
    MOV AX, 0
    CALL SetReference
    Call SetReferenceDeltaIntended
    CALL SetCurrentDirectionIntended    
    POP AX
    MOV BX, offset(Tx_Reset_String)
    MOV SI, offset(KP_Reset_Display_String)
    MOV AX, CS
    MOV ES, AX
    RET
SetupForAbsoluteReset ENDP

SetupForLaserOn PROC NEAR
        PUBLIC SetupForLaserOn
    CALL SetAwaitingAngleStatusFlag
	MOV AX, 0	
	CALL SetReferenceDeltaIntended
	; leave currentdirection intended as is.
    MOV BX, offset(Tx_LaserOn_String)
    MOV SI, offset(KP_LaserOn_Display_String)
    MOV AX, CS
    MOV ES, AX
    RET
SetupForLaserOn ENDP

SetupForLaserOff PROC NEAR
        PUBLIC SetupForLaserOff
    CALL SetAwaitingAngleStatusFlag
	MOV AX, 0	
	CALL SetReferenceDeltaIntended
	; leave currentdirection intended as is.
    MOV BX, offset(Tx_LaserOff_String)
    MOV SI, offset(KP_LaserOff_Display_String)
    MOV AX, CS
    MOV ES, AX
    RET
SetupForLaserOff ENDP

SetupForVelocityInc PROC NEAR
        PUBLIC SetupForVelocityInc
    CALL SetAwaitingAngleStatusFlag
	MOV AX, 0	
	CALL SetReferenceDeltaIntended
	; leave currentdirection intended as is.
    MOV BX, offset(Tx_SpeedIncrease_String)
    MOV SI, offset(KP_VelocityIncrease_Display_String)
    MOV AX, CS
    MOV ES, AX
    RET
SetupForVelocityInc ENDP

SetupForVelocityDec PROC NEAR
        PUBLIC SetupForVelocityDec
    CALL SetAwaitingAngleStatusFlag
	MOV AX, 0	
	CALL SetReferenceDeltaIntended
	; leave currentdirection intended as is.
    MOV BX, offset(Tx_SpeedDecrease_String)
    MOV SI, offset(KP_VelocityDecrease_Display_String)
    MOV AX, CS
    MOV ES, AX
    RET
SetupForVelocityDec ENDP

; UIAwaitingStatus clears ZF if awaiting an angle status update, sets otherwise
; from outside world's perspective, uses only flags register.
UIAwaitingStatus PROC NEAR
            PUBLIC UIAwaitingStatus
    PUSH AX
    PUSH BX
    MOV BX, offset(AwaitingAngleStatusFlag)    
    MOV AL, DS:[BX]
    CMP AL, 0               ; 0 means are not, 1 means are waiting
    JE NOT_AWAITING
    JNE AM_AWAITING

    NOT_AWAITING:
    CALL SetZF
    JMP UIAS_Done

    AM_AWAITING:
    CALL ClearZF
    JMP UIAS_Done

    UIAS_Done:
    POP BX
    POP AX
    RET
UIAwaitingStatus ENDP


SetAwaitingAngleStatusFlag PROC NEAR
        PUBLIC SetAwaitingAngleStatusFlag
    PUSH BX
    PUSH AX
    MOV BX, offset(AwaitingAngleStatusFlag)
    MOV AL, 1
    MOV DS:[BX], AL
    POP AX
    POP BX
    RET
SetAwaitingAngleStatusFlag ENDP

ClearAwaitingAngleStatusFlag PROC NEAR
        PUBLIC ClearAwaitingAngleStatusFlag
    PUSH BX
    PUSH AX
    MOV BX, offset(AwaitingAngleStatusFlag)
    MOV AL, 0
    MOV DS:[BX], AL
    POP AX
    POP BX
    RET
ClearAwaitingAngleStatusFlag ENDP

; takes offset in BX meaning DS:BX
; value in AX
; doesn't change registers except maybe flags
; writes low byte then high byte
SetWordAtOffset PROC NEAR
        PUBLIC SetWordAtOffset
    MOV DS:[BX], AL
    INC BX
    MOV DS:[BX], AH
    DEC BX
    RET
SetWordAtOffset ENDP

; takes offset in BX meaning DS:BX
; returns value in AX
GetWordAtOffset PROC NEAR
        PUBLIC GetWordAtOffset
    MOV AL, DS:[BX]
    INC BX
    MOV AH, DS:[BX]
    DEC BX
    RET
GetWordAtOffset ENDP

; takes new intended reference delta in AX
; writes to ReferenceDeltaIntended in DS
; doesn't change registers except possibly flags
SetReferenceDeltaIntended PROC NEAR
        PUBLIC SetReferenceDeltaIntended
    PUSH BX
    MOV BX, offset(ReferenceDeltaIntended)
    CALL SetWordAtOffset
    POP BX
    RET
SetReferenceDeltaIntended ENDP

; takes new intended current direction in AX
; writes to CurrentDirectionIntended in DS
; doesn't change registers except possibly flags
SetCurrentDirectionIntended PROC NEAR
        PUBLIC SetCurrentDirectionIntended
    PUSH BX
    MOV BX, offset(CurrentDirectionIntended)
    CALL SetWordAtOffset
    POP BX
    RET
SetCurrentDirectionIntended ENDP

; takes new reference angle in AX
; writes to Reference in DS
; doesn't change registers except possibly flags
SetReference PROC NEAR
        PUBLIC SetReference
    PUSH BX
    MOV BX, offset(Reference)
    CALL SetWordAtOffset
    POP BX
    RET
SetReference ENDP

; takes reported current direction in AX
; writes to CurrentDirection in DS
; doesn't change registers except possibly flags
SetCurrentDirection PROC NEAR
        PUBLIC SetCurrentDirection
    PUSH BX
    MOV BX, offset(CurrentDirection)
    CALL SetWordAtOffset
    POP BX
    RET
SetCurrentDirection ENDP



; gets ReferenceDeltaIntended in AX
; doesn't change registers except possibly flags
GetReferenceDeltaIntended PROC NEAR
        PUBLIC GetReferenceDeltaIntended
    PUSH BX
    MOV BX, offset(ReferenceDeltaIntended)
    CALL GetWordAtOffset
    POP BX
    RET
GetReferenceDeltaIntended ENDP

; gets CurrentDirectionIntended in AX
; doesn't change registers except possibly flags
GetCurrentDirectionIntended PROC NEAR
        PUBLIC GetCurrentDirectionIntended
    PUSH BX
    MOV BX, offset(CurrentDirectionIntended)
    CALL GetWordAtOffset
    POP BX
    RET
GetCurrentDirectionIntended ENDP

; gets Reference in AX
; doesn't change registers except possibly flags
GetReference PROC NEAR
        PUBLIC GetReference
    PUSH BX
    MOV BX, offset(Reference)
    CALL GetWordAtOffset
    POP BX
    RET
GetReference ENDP

; gets CurrentDirection in AX
; doesn't change registers except possibly flags
GetCurrentDirection PROC NEAR
        PUBLIC GetCurrentDirection
    PUSH BX
    MOV BX, offset(CurrentDirection)
    CALL GetWordAtOffset
    POP BX
    RET
GetCurrentDirection ENDP

; takes reported angle status in AX in 0,359
; checks ReferenceDeltaIntended.
;   if non-zero, were trying to change
;       reference angle. check Reference, add delta, check against
;       reported angle status - do they match? if so, it worked,
;       so return with ZF cleared. if not set zf and return.
;   if zero, check current direction intended (FBLR command previously sent)
;       if matches reported angle status, FLBR command worked.
;       return with ZF cleared. else return with ZF set.
; does not change any registers except flags.
UIIntentConsistentWithReportedStatus PROC NEAR
        PUBLIC UIIntentConsistentWithReportedStatus
    MOV CX, AX              ; all functions called here accept in AX and do not 
                            ; modify anything else.
    CALL GetReferenceDeltaIntended
    OR AX, AX
    JNZ TRIED_TO_CHANGE_REFERENCE
    JZ TRIED_TO_CHANGE_FLBR

    TRIED_TO_CHANGE_REFERENCE:
    MOV BX, AX                  ; store delta in BX
    CALL GetReference           ; reference in AX
    CALL AdditionMod360         ; AX <- AX + BX mod 360
    CMP AX, CX                  ; AX, CX in [0,359] so directly comparable
    JE ALL_IS_WELL
    JNE ANGLE_INCONSISTENCY

    TRIED_TO_CHANGE_FLBR:
    CALL GetCurrentDirectionIntended    ; get in AX
    CMP AX, CX                          ; in [0,359] guaranteed, directly comp
    JE ALL_IS_WELL
    JNE ANGLE_INCONSISTENCY

    ALL_IS_WELL:
    CALL ClearZF
    JMP DONE_VERIFIYING_STATUS
    
    ANGLE_INCONSISTENCY:
    CALL SetZF
    ;JMP DONE_VERIFIYING_STATUS

    DONE_VERIFIYING_STATUS:
    RET
UIIntentConsistentWithReportedStatus ENDP

; AX+BX mod 360 with AX, BX, positive, zero, or negative. Returns
; in AX in [0,359] and changes no other registers except flags.
; AX expected in [0,359] and |BX| = k <= 360 s.t. that overflow is not expected
; upon direct word addition and the result of a direct addition is 
; between [-360,719].
AdditionMod360 PROC NEAR
        PUBLIC AdditionMod360
    PUSH BX                     
    ADD AX, BX
    CMP AX, 0
    JL ADD_360_DEGREES
    CMP AX, 360
    JGE SUB_360_DEGREES
    JMP RETURN_ANGLE

    ADD_360_DEGREES:
    ADD AX, 360
    JMP RETURN_ANGLE

    SUB_360_DEGREES:
    SUB AX, 360
    ;JMP RETURN_ANGLE

    RETURN_ANGLE:               ; AX in [0,359]
    POP BX
    RET
AdditionMod360 ENDP

; updates UI-internal angle status, clears awaitingStatus
; should only be called if awaiting status, and reported motor status
; is consistent with what the UI board intended. 
; clears awaiting status flag. updates reference angle if referencedelta
; is nonzero (since we've already verified that the motor board
; has turned as desired). set current direction to intended current
; direction. 
; changes no registers except possibly flags.
UpdateUIAngleStatus PROC NEAR
        PUBLIC UpdateUIAngleStatus
    PUSH AX
    PUSH BX
    CALL ClearAwaitingAngleStatusFlag
    CALL GetReferenceDeltaIntended
    MOV BX, AX
    CALL GetReference
                        ; AX contains reference angle, BX contains delta
    CALL AdditionMod360
                        ; AX contains AX + BX mod 360
    CALL SetReference
    CALL GetCurrentDirectionIntended
    CALL SetCurrentDirection        ; since we verified the change was as
                                    ; intended
    POP BX
    POP AX
    RET
UpdateUIAngleStatus ENDP

CODE ENDS

;Shared Variables
DATA    SEGMENT PUBLIC  'DATA'

;(EO) boolean byte (0 no,1 yes)
Error_Occurred DB ?
;(CE) boolean byte (0 no, 1 yes)
Catastrophic_Error DB ?

; (WCEM) boolean byte (0 no, 1 yes)
Wrote_CE_Message DB ?

; boolean, 1, 0: are, (are not) awaiting a status update
AwaitingAngleStatusFlag DB ?

; step of CW CCW (+/- 5 degrees)
; if reference angle change was intended, 0 otherwise
ReferenceDeltaIntended DW ? 

; expected angle status returned after either CW CCW or FBLR (precalculated)
CurrentDirectionIntended DW ?

; current reference angle in UI board's understanding of things
Reference DW ?

; last value for angle from motor board
CurrentDirection DW ?

;  our EventBuffer word queue
;  maximum elements, QUEUE_SIZE-1
;  see SerialIO.inc
EventBuffer fixedLengthQueue  < >

; holds binary-to-decimal converted Dec2String output, with leading zeros
; length 7 to match Dec2String spec - sign, 5 digits, ASCII_NULL output
FLBRCommandArgument DB 7 DUP (?)

; holds command of the form FBLR, with argument, to be transmitted.
; Includes final place for ASCII_NULL so we can terminate our internal
; loops without explicit length constants for command strings.
FBLRCommandString DB 10 DUP (?)

DATA    ENDS

END