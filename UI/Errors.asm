NAME Errors
; Contains all error-message code strings used
; on either board and appropriate
; error-value mapping functions.
; Last Revision 12/29/2014 SSundaresh created

$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (SerialCS.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

; Description
; SerialIO currently maps errors read in the LSR 
; to pairings of the form, 
; SERIAL_ERROR_EVENT_ID, AND(LSR_COMPERROR, LSR).
; where 
; LSR_COMPERROR EQU 00011110B is a mask 

; This function identifies the highest priority
; LSR error bit, given the order below,

; 1 LSR_PARITYERR EQU 00000100B 
; 2 LSR_FRAMINGER EQU 00001000B
; 3 LSR_BREAKERRO EQU 00010000B
; 4 LSR_OVERRUNER EQU 00000010B 

; in the value passed in AL, which contains
; in the ordering above some mix
; of the error bits.

; Here we associates AL with the following
; ASCII digit, to be output as follows
; when EnqueueEvent's EventBuffer is 
; processed.

; SEP_ERROR Parity
; SEF_ERROR Frame
; SEB_ERROR Break
; SEO_ERROR Overrun
; These are given in priority. Only the first is output.

; This will be output in AL as the 
; new SerialError Value and returned.
; Register Used: AX
GET_SERIAL_ERROR_VALUE   PROC NEAR 
                PUBLIC GET_SERIAL_ERROR_VALUE
    MOV AH, LSR_PARITYERR
    AND AH, AL
    JNZ SET_SEP_ERROR
    MOV AH, LSR_FRAMINGER
    AND AH, AL
    JNZ SET_SEF_ERROR
    MOV AH, LSR_BREAKERRO
    AND AH, AL
    JNZ SET_SEB_ERROR
    MOV AH, LSR_OVERRUNER
    AND AH, AL
    JNZ SET_SEO_ERROR

    SET_SEP_ERROR:
    MOV AL, SEP_ERROR
    JMP GSEV_DONE
    SET_SEF_ERROR:
    MOV AL, SEF_ERROR
    JMP GSEV_DONE
    SET_SEB_ERROR:
    MOV AL, SEB_ERROR
    JMP GSEV_DONE    
    SET_SEO_ERROR:
    MOV AL, SEO_ERROR
    JMP GSEV_DONE

    GSEV_DONE:
    RET
GET_SERIAL_ERROR_VALUE ENDP


; Motor_Error_String
; Template for the Motor Error String.
; Includes an ASCII null character 
; where the error code should be inserted
; before transmitting.
;
; Author:           SSundaresh
; Last Modified:    12/29/2014

Motor_Error_String		LABEL   BYTE
						PUBLIC  Motor_Error_String
	DB ASCII_CR 
	DB 'R'
	DB ASCII_NULL
	DB ASCII_CR 	


; UI Display Error Messages
; 7 segment
; Author:           SSundaresh
;                   12/29/2014 created for motor board
; Last Modified:    12/31/2014 added UI errors for
;                   unexpected status updates
;                   consolidated with Display Error Message

; DisplayErrorMessage String, the error to be displayed on the LED display 
; when a string of invalid length is passed to DisplayFiles.Display.
DisplayErrorMessage LABEL BYTE
                   PUBLIC DisplayErrorMessage
    DB 'E'
	DB 'r'
	DB 'r'
	DB '-'
	DB 'l'
	DB 'E'
	DB 'n'			
	DB '-'
	DB 'D'
	DB 'i'
	DB 'S'
	DB 'P'
    DB  ASCII_NULL

Motor_Serial_Error_String		LABEL   BYTE
						PUBLIC  Motor_Serial_Error_String
	DB 'c'
	DB 'A'
	DB 'r'
	DB '-'
	DB 'S'
	DB 'E'
	DB 'r'			
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	    

UI_Serial_Error_String		LABEL   BYTE
						PUBLIC  UI_Serial_Error_String
	DB 'U'
	DB 'i'	
	DB '-'
	DB 'S'
	DB 'E'
	DB 'r'			
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	        

Motor_Parse_Error_String		LABEL   BYTE
						PUBLIC  Motor_Parse_Error_String
	DB 'c'
	DB 'A'
	DB 'r'
	DB '-'
	DB 'P'
	DB 'A'
	DB 'r'
	DB 'S'	
	DB 'E'
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	    

UI_Parse_Error_String		LABEL   BYTE
						PUBLIC  UI_Parse_Error_String
	DB 'U'
	DB 'i'	
	DB '-'
	DB 'P'
	DB 'A'
	DB 'r'
	DB 'S'	
	DB 'E'
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	    

Motor_Overflow_Error_String		LABEL   BYTE
						PUBLIC  Motor_Overflow_Error_String
	DB 'c'
	DB 'A'	
	DB 'r'	
	DB '-'
	DB 'b'
	DB 'U'
	DB 'F'
	DB 'F'	
	DB 'E'
	DB 'r'
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	     

UI_Overflow_Error_String		LABEL   BYTE
						PUBLIC  UI_Overflow_Error_String
	DB 'U'
	DB 'i'		
	DB '-'
	DB 'b'
	DB 'U'
	DB 'F'
	DB 'F'	
	DB 'E'
	DB 'r'
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL	

UI_UnexpectedMotorStatus_Error_String LABEL BYTE
            PUBLIC UI_UnexpectedMotorStatus_Error_String
	DB 'c'
	DB 'A'		
	DB 'r'		
	DB '-'
	DB 'U'
	DB 'i'	
	DB '-'
	DB 'S'
	DB 't'
	DB 'A'	
	DB 't'		
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL   

UI_MotorStatusInconsistent_Error_String LABEL BYTE
            PUBLIC UI_MotorStatusInconsistent_Error_String
	DB 'c'
	DB 'A'		
	DB 'r'		
	DB '-'
	DB 'U'
	DB 'i'	
	DB '-'
	DB 'S'
	DB 't'
	DB 'A'	
	DB 't'		
	DB '-'
	DB 'E'
	DB 'r'
	DB 'r'	
    DB ASCII_NULL   
CODE ENDS


END