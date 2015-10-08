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