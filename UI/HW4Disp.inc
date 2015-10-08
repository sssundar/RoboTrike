; Name HW4DisplayConstants.inc
; Description: Initialization constants for Display function local variables 
;    and structures in DisplayFiles.asm. Also includes relevant register
;    IO-space output constants to write to the LED 7-segment display array.
;
;
; Note: these functions fail if PACS/MPCS set up to shift IO address
; base away from 0.
;
; Revision History: 
;  11/14/2014 SSundaresh created
;  12/31/2014 SSundaresh consolidated ASCII constants with GenConst.inc
; 						 updated ScrollBufferLen to be able to display
; 						longer error messages, scrolling.
; 						noticed that shifting IO address base would
; 						cause error based on how Display EH works.

; Empty string has length 0
EMPTY_STRING_LENGTH EQU 0
 
; arbitrarily allow 2^n = 16 characters as max string length, so we can
; use 2^n-1 modulus mask trick from HW3
SCROLL_BUFFER_LENGTH EQU 16
SCROLL_MODULUS_MASK EQU SCROLL_BUFFER_LENGTH - 1 

; how many 7-segment LEDs we have, and associated mask via same trick
; see Limitations of DisplayFiles.asm
; MAX_DISPLAY_WIDTH is required less than 255. 
MAX_DISPLAY_WIDTH EQU 8
DISPLAY_MODULUS_MASK EQU MAX_DISPLAY_WIDTH - 1
 
; For scrolling, what's the frequency of the scroll in shifts/second?
; Choose SCROLL_COUNTER_MODULO_MASK to be some 2^n so modulo wrapping 
; by AND 2^n -1 is simple.
; We choose 512 arbitrarily, since want a 1KHz refresh rate, and 
; a ~0.5Hz scroll rate seems reasonable.
SCROLL_COUNTER_MODULO_MASK EQU 511
SCROLL_COUNTER_RESET EQU 0

; Scrolling_Buffer structure   
; AmScrolling: if 1, true, if 0, false. 
; ScrollTimer: will hold count till SCROLL_MAX_COUNTER
; BasePosition: Position in buffer to scroll from
; RelativePosition: The current character being displayed, relative to 
;  the base position. This is less than or equal to MAX_DISPLAY_WIDTH.
;   Both BasePos and Relative Pos need to be wrapped; BasePos to 
;   the length of LEDSegmentPatterns, and RelativePos to MAX_DISPLAY_WIDTH.
;   Note RelativePosition is also the current LED we should be on
;   and that RelativePos will fit in a byte by the assumption on
;   MAX_DISPLAY_WIDTH above.
; LEDSegmentPatterns: array that holds the 7-segment patterns to display.
; Note 7-segment patterns are bytes in segtable.asm.
SCROLLING_BUFFER    STRUC
    AmScrolling         DB ?
    ScrollTimer         DW ?
    BasePos             DW ?
    RelativePos         DW ?
    LEDSegmentPatterns  DB SCROLL_BUFFER_LENGTH DUP (?)
SCROLLING_BUFFER    ENDS

; Related initialization constants
AmNotScrolling EQU 0
AmTooScrolling EQU 1

; a length-6 byte array to hold the result of conversions from Dec2String,
; Hex2String. Their outputs are max length 5 plus sign, plus one terminator.
MAX_CONVERSION_SIZE EQU 6
ConversionsOutputBuffer STRUC
    CharArray           DB MAX_CONVERSION_SIZE DUP (?)
ConversionsOutputBuffer ENDS
           

       