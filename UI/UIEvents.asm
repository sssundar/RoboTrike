NAME Errors
; Contains display messages for user interaction events
; for UI board. Contains command templates
; for transmission on keypad events.
; Last Revision 12/31/2014 SSundaresh created

$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (SerialCS.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

; template for UI-to-Motor board commands
; ASCII_NULL at the end is not
; to be transmitted, but it tells us
; when to stop looking for characters
; to transmit. 

Tx_LaserOn_String LABEL BYTE
            PUBLIC Tx_LaserOn_String
    DB ASCII_CR
    DB 'f'
    DB ASCII_CR    
    DB ASCII_NULL
Tx_LaserOff_String LABEL BYTE
            PUBLIC Tx_LaserOff_String
    DB ASCII_CR
    DB 'o'
    DB ASCII_CR    
    DB ASCII_NULL

; increase angle by set number of degrees
Tx_AngleIncrease_String LABEL BYTE
            PUBLIC Tx_AngleIncrease_String
    DB ASCII_CR
    DB 'd'
    DB '+'
    DB LeftwardTurnAngleChar    
    DB ASCII_CR  
    DB ASCII_NULL  

; decrease angle by set number of degrees
Tx_AngleDecrease_String LABEL BYTE
            PUBLIC Tx_AngleDecrease_String
    DB ASCII_CR
    DB 'd'
    DB '-'
    DB RightwardTurnAngleChar    
    DB ASCII_CR  
    DB ASCII_NULL  

; increase speed by 500 units
Tx_SpeedIncrease_String LABEL BYTE
            PUBLIC Tx_SpeedIncrease_String
    DB ASCII_CR
    DB 'v'
    DB '+'
    DB '5'    
    DB '0'    
    DB '0'    
    DB ASCII_CR  
    DB ASCII_NULL  

Tx_SpeedDecrease_String LABEL BYTE
            PUBLIC Tx_SpeedDecrease_String
    DB ASCII_CR
    DB 'v'
    DB '-'
    DB '5'    
    DB '0'    
    DB '0'    
    DB ASCII_CR  
    DB ASCII_NULL  

; Reset
Tx_Reset_String LABEL BYTE
            PUBLIC Tx_Reset_String
    DB ASCII_CR
    DB 'r'    
    DB ASCII_CR  
    DB ASCII_NULL 

; Forward template
; 0s can be replaced.
; expect to be passing a Dec2String
; converted result, hence +##### with
; leading zeros.
Tx_FBLR_Template_String LABEL BYTE
            PUBLIC Tx_FBLR_Template_String
    DB ASCII_CR
    DB 'd'    
    DB '0'    
    DB '0'    
    DB '0'    
    DB '0'    
    DB '0'    
    DB '0'    
    DB ASCII_CR  
    DB ASCII_NULL 

; UI Keypad Press Display Messages
; 7 segment
; Author:           SSundaresh
; Last Modified:    12/31/2014 created
KP_Forward_Display_String LABEL BYTE
            PUBLIC KP_Forward_Display_String
    DB 'F'
    DB ASCII_NULL
KP_Backward_Display_String LABEL BYTE
            PUBLIC KP_Backward_Display_String
    DB 'b'
    DB ASCII_NULL
KP_Left_Display_String LABEL BYTE
            PUBLIC KP_Left_Display_String
    DB 'l'
    DB ASCII_NULL
KP_Right_Display_String LABEL BYTE
            PUBLIC KP_Right_Display_String
    DB 'r'
    DB ASCII_NULL    

KP_Reset_Display_String LABEL BYTE
            PUBLIC KP_Reset_Display_String
    DB 'r'
	DB 'E'
	DB 'S'
	DB 'E'
	DB 't'
    DB ASCII_NULL    

KP_LaserOn_Display_String LABEL BYTE
            PUBLIC KP_LaserOn_Display_String
    DB 'F'
	DB 'i'
	DB 'r'
	DB 'E'
	DB '-'
	DB 'L'
	DB 'A'
	DB 'S'
	DB 'E'
	DB 'r'
    DB ASCII_NULL    
	
KP_LaserOff_Display_String LABEL BYTE
            PUBLIC KP_LaserOff_Display_String
    DB 'l'
	DB 'A'
	DB 'S'
	DB 'E'
	DB 'r'
	DB '-'
	DB 'O'
	DB 'F'
	DB 'F'
    DB ASCII_NULL    

KP_AngleIncrease_Display_String LABEL BYTE
            PUBLIC KP_AngleIncrease_Display_String
    DB 't'
	DB 'u'
	DB 'r'
	DB 'n'
	DB '-'
	DB 'l'
	DB 'E'
	DB 'F'
	DB 't'
    DB ASCII_NULL

KP_AngleDecrease_Display_String LABEL BYTE
            PUBLIC KP_AngleDecrease_Display_String
    DB 't'
	DB 'u'
	DB 'r'
	DB 'n'
	DB '-'
	DB 'r'
	DB 'i'
	DB 'g'
	DB 'h'
	DB 't'
    DB ASCII_NULL

KP_VelocityIncrease_Display_String LABEL BYTE
            PUBLIC KP_VelocityIncrease_Display_String
    DB 'S'
	DB 'P'
	DB 'E'
	DB 'E'
	DB 'D'
	DB '-'
	DB 'i'
	DB 'n'
	DB 'c'
    DB ASCII_NULL    
KP_VelocityDecrease_Display_String LABEL BYTE
            PUBLIC KP_VelocityDecrease_Display_String
    DB 'S'
	DB 'P'
	DB 'E'
	DB 'E'
	DB 'D'
	DB '-'
	DB 'd'
	DB 'E'
	DB 'c'
    DB ASCII_NULL


CODE ENDS


END