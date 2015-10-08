; Name Events.inc
; Contains all non-error event identifiers, values.
; Last Revised: 12/31/2014 SSundaresh consolidated from 
; 						rest of code for HW9

; All motor board error events have the
; same ID, with numerical values in 1-9
SERIAL_ERROR_EVENT_ID EQU 00000010B

; Note that Parser errors, or EventBuffer overflow errors
; are handled separately in ProcessEvents and EnqueueEvent
; as described in Handlers.asm

; Serial Character Received Event; codes are just the characters
SERIAL_CHAR_EVENT_ID EQU 00000100B

; Values of various error events.
; Character events are their own values.
; These will be Txd as \CR R X_ERROR \CR
; 1  	Processing Error - EnqueueEvent Buffer Full (EBF)
; 2 	Serial Error - Parity (SEP)
; 3 	Serial Error - Frame (SEF)
; 4 	Serial Error - Break (SEB)
; 5 	Serial Error - Overrun (SEO)
; 6 	General Parsing Error (GPE)
; If multiple serial errors occur we'll take them
; in the order above, whichever comes first.
EBF_ERROR EQU '1'
SEP_ERROR EQU '2'
SEF_ERROR EQU '3'
SEB_ERROR EQU '4'
SEO_ERROR EQU '5'
GPE_ERROR EQU '6'



; keypress event identifier for enqueueevent
EVENT_KEYPRESS 	EQU 00000001B

; key press event values (add row,col)

; values will be stored in arrays for extensibility.
; the two constants below tell us the size of the arrays.
numSupportedKeyPresses EQU 4
numRows 			   EQU 4 ; required of the form 2^n for mask-wrapping

; row values

ROW0VAL EQU 00000000B
ROW1VAL EQU 00010000B
ROW2VAL EQU 00100000B
ROW3VAL EQU 00110000B

; row IO space addresses (top to bottom, with power cable at top left)
ROW0_IO_ADDRESS EQU 80H
ROW1_IO_ADDRESS EQU 81H
ROW2_IO_ADDRESS EQU 82H
ROW3_IO_ADDRESS EQU 83H

; supported single-key press column io readins
; column values (the 4bit value active low values read in for each column)
COL_LEFT_VAL 		EQU 00001110B
COL_MIDLEFT_VAL 	EQU 00001101B
COL_MIDRIGHT_VAL 	EQU 00001011B
COL_RIGHT_VAL 		EQU 00000111B

UNPRESSED_COLUMN_VALUES EQU 00001111B ; no switches pressed, all high.
 									  ; also an AND mask for the IO readin
 									  ; to get only the true keypress bits.

; Keypad event value to UI board action mapping constants
; given SupportedPresses, single-key, these
; are the possible keypad press event values
; that could be enqueued via EnqueueEvent
Keypad_Row0_L 	EQU ROW0VAL + COL_LEFT_VAL
Keypad_Row0_ML 	EQU ROW0VAL + COL_MIDLEFT_VAL
Keypad_Row0_MR	EQU ROW0VAL + COL_MIDRIGHT_VAL
Keypad_Row0_R  	EQU ROW0VAL + COL_RIGHT_VAL

Keypad_Row1_L 	EQU ROW1VAL + COL_LEFT_VAL
Keypad_Row1_ML 	EQU ROW1VAL + COL_MIDLEFT_VAL
Keypad_Row1_MR	EQU ROW1VAL + COL_MIDRIGHT_VAL
Keypad_Row1_R  	EQU ROW1VAL + COL_RIGHT_VAL

Keypad_Row2_L 	EQU ROW2VAL + COL_LEFT_VAL
Keypad_Row2_ML 	EQU ROW2VAL + COL_MIDLEFT_VAL
Keypad_Row2_MR	EQU ROW2VAL + COL_MIDRIGHT_VAL
Keypad_Row2_R  	EQU ROW2VAL + COL_RIGHT_VAL

Keypad_Row3_L 	EQU ROW3VAL + COL_LEFT_VAL
Keypad_Row3_ML 	EQU ROW3VAL + COL_MIDLEFT_VAL
Keypad_Row3_MR	EQU ROW3VAL + COL_MIDRIGHT_VAL
Keypad_Row3_R  	EQU ROW3VAL + COL_RIGHT_VAL

;		Keypad mapping: 
;    	L  ML  MR   R 	 Columns
;			Fo 		Reset	0 Row
;		Le 		Ri 	LON		1 Row
;			Ba 		LOFF 	2 Row
;		VI  VD  CCW CW  	3 Row

; Key presses with meaning
KP_Forward 				EQU Keypad_Row0_ML
KP_Backward 			EQU Keypad_Row2_ML
KP_Left 				EQU Keypad_Row1_L
KP_Right 				EQU Keypad_Row1_MR
KP_Reset 				EQU Keypad_Row0_R
KP_LaserOn 				EQU Keypad_Row1_R
KP_LaserOff 			EQU Keypad_Row2_R
KP_VelocityIncrease 	EQU Keypad_Row3_L
KP_VelocityDecrease 	EQU Keypad_Row3_ML
KP_AngleIncrease 		EQU Keypad_Row3_MR
KP_AngleDecrease 		EQU Keypad_Row3_R
; Increase CCW, decrease CW

LeftwardTurnAngleChar  EQU '5'
RightwardTurnAngleChar EQU '5'
LeftwardTurnAngle EQU 5
RightwardTurnAngle EQU -5
