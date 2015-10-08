Name DFiles
; Description: Contains the functions 
;   Display         (MuxDisplays passed string on 7-segment LED array)
;       DisplayHex  (MuxDisplays passed 16-bit unsigned value in hex)
;       DisplayNum  (MuxDisplays passed 16-bit signed value in signed decimal)
;  Display_Mux_7Segment(workhorse; cycles through patterns we intend to display
;                       and drives one 7-segment LED at a time to provide
;                       an illusion of continuity.)
;   InitDisplay     (sets up local vars & chip-selects to access LEDs).
;
; Limitations: These functions do not handle more than 256 LED segments/array,
;              They can handle up to 2^16 scrolled chars.
;              Moreover these LEDs must be addressable as 00H, 01H, ... FFH.
; Revision History:
; 11/15/2014 SSundaresh TOC written




; For descriptions of constants and structures see the include file below.
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (HW4Disp.inc)    

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP


; Numerical conversion functions to hexadecimal and signed decimal.
EXTRN   Hex2String:NEAR, Dec2String:NEAR

; 7-segment LED ASCII-segment encoding. Segment patterns are words, SegTable
; access via byte index in 0-127.
EXTRN   ASCIISegTable:BYTE 

; Code-segment error messages for display; maximum length MAX_DISPLAY_WIDTH
; Indices in EMPTY_STRING_LENGTH to MAX_DISPLAY_WIDTH-1
EXTRN   DisplayErrorMessage:BYTE

        
; Functional Specification: Display
; Description  
; Takes a null terminated character array (string) of length
; less than or equal to SCROLL_BUFFER_LENGTH, passed by reference in ES:[SI]. 
; This string is char-by-char encoded in 7-segment LED patterns and written
; to a local buffer, which is displayed on a MAX_DISPLAY_WIDTH 7-segment
; LED array, serially, in quick succession, by Display_Mux_7Segment
; upon Timer2 interrupt
; 
; Lengths that follow exclude the null terminator. Anything past a null                
; terminator is ignored and will not be displayed.
; A null string (length 0), clears the local buffer, which rapidly clears the          
; display.
;
; A string of length <= MAX_DISPLAY_WIDTH is extended to MAX_DISPLAY_WIDTH          
; with null terminators (cleared 7-segment LEDs).
;
; A string of MAX_DISPLAY_WIDTH < length <= SCROLL_BUFFER_LENGTH is extended to     
; the upper limit by null terminators then scrolled at frequency 
; Timer2 Interrupts / SCROLL_MAX_COUNTER. 
;
; Larger strings are invalid and will result an error message being displayed.          
; 
; Operation
; Take an ASCII character buffer.
; Counting the length L up to but not including the terminator.
;
; If SCROLL_BUFFER_LENGTH < L, set the ScrollBuffer scrolling flag, AmScrolling.
; Toss the passed string, and setup to ouput a MAX_DISPLAY_WIDTH length
; error message. Proceed with this new string.
;
; IF L = 0, fill the buffer with nulls then exit.
; IF 0 < L <= MAX_DISPLAY_WIDTH, we don't need to scroll and set the
; scroll flag appropriately. We just need to FillInSegments corresponding
; to the string characters then top off with nulls in the ScrollBuffer. 
; If MAX_DISPLAY_WIDTH < L <= SCROLL_BUFFER_LENGTH we set
;   ScrollBuffer.AmScrolling active and FillInSegments as before.
;
; To FillInSegments, we use a poor-man's equivalent of XLAT (manual)
; to fill ScrollBuffer with segments from segtable.asm appropriate to the 
; chars. Note that 7-segment patterns are bytes. 
;
; To top off with nulls, we jump to NULL_FILLER then loop from L to                     
; SCROLL_BUFFER_LENGTH to fill ScrollBuffer with 7-segment equivalent
; to ASCII_NULL segment equivalents.
; 
; Finally, we reset BasePos and RelativePos to 0. 
; 
; The Display_Mux_7Segment function and Timer2 Interrupts will take care of 
; everything else.
;         
; Arguments: Takes a null terminated character array (string) of length
; less than or equal to SCROLL_BUFFER_LENGTH, passed by reference in ES:[SI]. 
;			 
; Return Values:    None
;
; Local Variables: 
;  DI: Length L of the string array passed.
;  BX: Local holder of characters and segtable lookups.
;  CX: Local holder of SI for mutation
;  ES: Segment of the string array to display.
;  SI: Offset of the string array to display.  
;
; Constants: 		
; AmNotScrolling
; AmTooScrolling (see operation)
; DisplayErrorMessage, see Error Handling. 
; SCROLL_BUFFER_LENGTH, max string length for scrolling.
; MAX_DISPLAY_WIDTH, a length past which scrolling is necessary.
; EMPTY_STRING_LENGTH
;
; Shared Variables: 
; ScrollBuffer, mutated by Display with the 14-segment patterns associated 
; with the characters in the string passed, and written to the display
; by MuxDisplay.
;
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   Input strings of length greater than SCROLL_BUFFER_LENGTH
;                   will not be displayed. DisplayErrorMessage will be shown. 
;
; Registers Used:   AX, BX, CX, ES, SI, DX, DI, flags.
; Stack Depth:      0 words
;
; Algorithms:       None
;
; Data Structures:  Circular buffer for scrolling, 2^n-1 AND MASK modulus trick
;                   from HW3.
;
; Known Bugs:       None
; Limitations:      If interrupted by another call to Display
;                   which changes the scroll buffer, will likely show garbage.
;                   Will break if you pass in a non-terminated string.                   
; Revision History:					
;   11/14/2014  SSundaresh created

Display PROC NEAR
        PUBLIC Display
    PUSHA
    MOV CX, SI                          ; hold SI for later.
    MOV DI, EMPTY_STRING_LENGTH         ; DX holds Length
    FIND_STRING_LENGTH:             
        MOV BL, ES:[SI]                   ; get next character in BL
        INC SI
        INC DI                          ; increment length count by one segment
                                        ; this is equivalent to byte count
                                        ; for 7-segment displays.                                     
        CMP BL, ASCII_NULL              ; have we hit the null terminator?  
        JZ SCROLL_AND_ERROR_HANDLING    ; yes
        JNZ FIND_STRING_LENGTH          ; no, keep looping
    
    SCROLL_AND_ERROR_HANDLING:
        DEC DI                          ; counting from zero, DX now holds 
                                        ; segment length sans null terminator.
        
        CMP DI, SCROLL_BUFFER_LENGTH
        JLE LENGTH_ALL_RIGHT             ; if L > SCROLL_BUFFER_LEN, error.
        MOV DX, CS
        MOV ES, DX                       ; Error to display is in CS
        MOV SI, OFFSET(DisplayErrorMessage) ; New string is DisplayErrorMessage
        MOV CX, SI
        MOV DI, MAX_DISPLAY_WIDTH        ; L = MAX_DISPLAY_WIDTH        
                                         ; proceed as normal
        LENGTH_ALL_RIGHT:
        CMP DI, EMPTY_STRING_LENGTH
        JE NULL_FILLER                  ; if L = 0, just fill all nulls, exit.
        
        MOV BL, AmTooScrolling        
        MOV ScrollBuffer.AmScrolling, BL ; assume L > MAX_DISPLAY_WIDTH, will
                                         ; need to scroll
        CMP DI, MAX_DISPLAY_WIDTH
        JG FILL_IN_SEGMENTS             ; we were right
        MOV BL, AmNotScrolling ; we were wrong, change AmScrolling
        MOV ScrollBuffer.AmScrolling, BL 
                
    FILL_IN_SEGMENTS:
        MOV SI, CX                          ; let's get SI back to base of str
        MOV DI, EMPTY_STRING_LENGTH         ; DI now counts ScrollBuffer 
                                            ; RelativeIndex. Can't change that
                                            ; variable directly as timer 
                                            ; interrupts and Mux calls
                                            ; are being made this entire time,
                                            ; and they also mutate RelPos.
                                        ; this time, am sure string is valid.       
        TRANSLATE_CHAR_TO_SEGMENTS:     ; guaranteed to have at least 1 char to
                                        ; encode. 
            MOV AL, ES:[SI]             ; get next char byte from string
            CMP AL, ASCII_NULL 
            JZ NULL_FILLER              ; if null, we're done with string.
                                        ; otherwise..
                                        
            LEA BX, ASCIISegTable       ; XLAT wasn't working for me.         
            XOR AH, AH
            ADD BX, AX
            MOV AL, CS:[BX] 
            
                                    ; AL now contains the segment equiv.
                                    ; of the char at ES:[SI]
            
            MOV ScrollBuffer.LEDSegmentPatterns[DI], AL ; write to ScrollBuffer
            INC DI                      ; increment ScrollBuffer load index
            INC SI                      ; increment String index
            JMP TRANSLATE_CHAR_TO_SEGMENTS ; keep looping.
    
    NULL_FILLER:                        ; Whether we got here from
                                        ; seeing an empty string (L = DI = 0)
                                        ; or reaching a null after some XLAT,
                                        ; (DI = L + 1), DI at this point 
                                        ; indexes a spot that should be filled
                                        ; with nulls till the buffer's end.
        CMP DI, SCROLL_BUFFER_LENGTH        
        JGE RESET_POSITIONING
        MOV AL, ASCII_NULL
        MOV ScrollBuffer.LEDSegmentPatterns[DI], AL
        INC DI
        JMP NULL_FILLER
    
    RESET_POSITIONING:
        MOV ScrollBuffer.ScrollTimer, 0
        MOV ScrollBuffer.BasePos, 0     

    DISPLAY_FINISHED:
    POPA
    RET
Display ENDP
   

; Functional Spec, DisplayHex
; Description
; Takes a 16 bit unsigned input, n, by register, asks external function
; Hex2String to convert it into Hex and store it in a local string buffer.
; This representation has (no sign, leading zeros). It is at most 4 digits.
; This buffer is then passed to Display for display. 
;
; Operation
; n is passed in AX, just as Hex2String requires.
; Where we want to converted hex string representation to be stored is in
; a shared string buffer ConvertsBuffer. We force DS:[SI] to point to this
; by setting SI appropriately. 
; We push DS, SI registers before the function call just to be safe. We don't
; need to save any other registers.
; We call Hex2String, then we pop SI, DS.
; DS:SI now points to ConvertsBuffer filled with a converted hexadecimal string
; terminated by a null ASCII character.
; so we ask ES to point to DS and simply call Display. 
; In principle here too we'd have to push/pop but there's nothing left to do.
;
; 
; Arguments: Takes n passed by value in AX, n 16 bit unsigned.
;			 
; Return Values:    None
;
; Local Variables: 
;  AX: n
;  ES: points to DS
;  SI: Offset of the string array to display.  
;
; Constants: 	None
;
; Shared Variables: 
; ConvertsBuffer, the string array shared variable mutated by Hex2String
; and passed to Display.
;
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   AX, ES, SI, BX
; Stack Depth:      2 words
;
; Algorithms:       None
;
; Data Structures:  None
;
; Known Bugs:       None
; Limitations:      Limited conversion size; 16 bits, 4 hex digits.
;
; Revision History:					
;   11/15/2014  SSundaresh created
DisplayHex PROC NEAR
            PUBLIC DisplayHex
    MOV SI, OFFSET(ConvertsBuffer)
    PUSH DS
    PUSH SI
    CALL Hex2String
    POP SI    
    POP DS
    DEBUG1:
    MOV BX, DS
    MOV ES, BX
    CALL Display
    RET
DisplayHex ENDP
        
; Functional Spec, DisplayNum
; Description
; Takes a 16 bit signed input, n, by register, asks external function
; Dec2String to convert it into signed decimal nd store it in a local buffer.
; This representation has (sign char, leading zeros). It is at most 6 chars
; including sign. 
; This buffer is then passed to Display for display. 
;
; Operation
; n is passed in AX, just as Dec2String requires.
;
; Other than that see the Functional Spec for DisplayHex.
;
; Limitations:      Limited conversion size; 16 bits, 5 decimal digits max.
;
; Revision History:					
;   11/15/2014  SSundaresh created
DisplayNum PROC NEAR
        PUBLIC DisplayNum
    MOV SI, OFFSET(ConvertsBuffer)
    PUSH DS
    PUSH SI
    CALL Dec2String
    POP SI
    POP DS
    MOV BX, DS
    MOV ES, BX
    CALL Display
    RET
DisplayNum ENDP

; Functional Spec, Display_Mux_7Segment
; Description
; Display IO initialization is handled in DisplayInit.
; This function is called by the Timer2 Event Handler in TimerFiles.asm.
; Thus this function itself does not deal with interrupt protocol.
;
; Its purpose is to software multiplex the display of the LED segment
; patterns in ScrollBuffer, written by Display calls.
; 
; When called, if the current ScrollBuffer scroll flag is not set, 
; then this display mux simply updates its relative position within the
; current ScrollBuffer, gets the next segment pattern to display,
; and drives the same relative LED segment position in the display array. 
;
; If the scroll flag is set, this display mux increments a scroll timing 
; counter. When SCROLL_COUNTER_MODULO_MASK+1 is hit, the BasePosition we're at
; should be shifted right along the string (i.e. scroll).
; Otherwise we simply treat this as a non-scrolling mux display,
; looping over the same MAX_DISPLAY_WIDTH values in our string, from the
; current BasePosition, incrementing our RelativePosition on each interrupt. 
; This works out to displaying the same string 
; for SCROLL_COUNTER_MODULO_MASK+1 / Timer2 Frequency seconds, then scrolling 
; forward one character. 
;
; Note that BasePositions must be wrapped by SCROLL_MODULUS_MASK
; and RelativePositions by DISPLAY_MODULUS_MASK, as the
; ScrollBuffer is circular, and the RelativePositions must fit within
; the DisplayWidth. Similarly the ScrollTimer must be wrapped by
; SCROLL_COUNTER_MODULO_MASK.
;
; This function cannot (read: should not) be interrupted by another Mux call.
; By previous assumptions on MAX_DISPLAY_WIDTH, detailed at the top of this
; document, it will fail for MAX_DISPLAY_WIDTH > 255 and if IO space is not
; mapped as assumed.
; 
; Operation
; Get ScrollBuffer Structure in DX.
; Check if we are scrolling. 
; If not,
    ; jump directly to the NON_SCROLLING_DISPLAY_MUX
; If we are,
    ; Increment the ScrollBuffer.ScrollTimer
        ; (We do not use another timer for this because we don't know how
        ; many more timers we'll need for the rest of the project).
    ; Wrap ScrollTimer by SCROLL_COUNTER_MODULO_MASK via the AND trick from HW3
    ; Update ScrollTimer
    
    ; If this wrapped count is zero, 
        ; Get BasePos to scroll from in CX
        ; increment BasePos (scroll forward 1 char)
        ; Wrap this BasePos by SCROLL_MODULUS_MASK
        ; Store new BasePos in ScrollBuffer.BasePos
        ; Our next segment pattern to display is now exactly 
        ; ScrollBuffer.LEDSegmentPattern[CX].
        ; Jump to OUT_SEGPATTERN
    ; If the wrapped count is not zero, 
        ; Jump to NON_SCROLLING_DISPLAY_MUX
    
; NON_SCROLLING_DISPLAY_MUX simply
    ; Gets the RelativePosition in CX.
    ; Increments and wraps by DISPLAY_MODULUS_MASK
    ; Stores new RelativePosition
    ; Gets untouched BasePos in BX
    ; Adds to updated RelativePosition.
    ; Wraps by SCROLL_MODULUS_MASK. 
    ; Now CX has the offset (relative to ScrollBuffer.LEDSegmentPatterns)
    ; of the next segment pattern we want to display.
    ; Jumps to OUT_SEGPATTERN

; OUT_SEGPATTERN 
    ; simply needs to get ScrollBuffer.LEDSegmentPattern[CX] into AL then drive
    ; the ScrollBuffer.RelativePos'th LED segment in the display array
    ; with this segment pattern.
    ; It does this as follows:
    ; It moves ScrollBuffer.RelativePos [in 0-Bto a byte register, AH
    ; Now AH contains a byte that exactly addresses one LED 7-segment element
    ; given how our 80188 is wired up to our LEDs, and using fixed-IO addresses
    ; Then we just ask OUT AH, AL. 
    ; This assumes MPCS, PACS set up appropriately in HW4MainLoop.asm.    
; Finished, we can return.
;
; Arguments: None 
;			 
; Return Values: None
;
; Local Variables: 
;  
; Constants: 	
; AmTooScrolling: Scroll flag active constant, for testing current flag.
; SCROLL_COUNTER_MODULO_MASK: software timer-max count for scrolling delay.
; SCROLL_MODULUS_MASK: used for circular wrapping of BasePos.
; DISPLAY_MODULUS_MASK: used for circular wrapping of RelativePos.
;
; Shared Variables: ScrollBuffer, in Data Segment. Mutated with segment
;   patterns by Display, read and displayed by this function.
;
; Global Variables: None
;
; Input:            None
; Output:           Drives 7-segment LED display array of length MAX_DISPLAY_
;                   WIDTH subject to the Limitations below, using patterns
;                   in ScrollBuffer.
;
; Error Handling:   None
;
; Registers Used:   AX, CX, DX, DI
; Stack Depth:      0 words
;
; Algorithms:       None
;
; Data Structures:  None
;
; Known Bugs:      None
; Limitations:     This code assumes that the LED segments are addressed
;                  in IO space from the PCS base as 00, 01, ... , 07...
;                  It does not handle more than 255 such LED segments.
;                  These are magic numbers.
;                  That's the only reason RelativePos is a proxy for 
;                  the fixed offset IO addressing used with OUT. 
;                  For a different setup, we'd have to create a SegTable to map
;                  RelativePosition to the IO address of each segmented display.
;
; Revision History:					
;   11/15/2014  SSundaresh created
Display_Mux_7Segment PROC NEAR  
                    PUBLIC Display_Mux_7Segment
    MOV AL, AmTooScrolling
    TEST ScrollBuffer.AmScrolling, AL   ; Are we scrolling?    
          
     JZ NON_SCROLLING_DISPLAY_MUX        ; if not..
     JNZ SCROLLING_DISPLAY_MUX           ; if yes..

    SCROLLING_DISPLAY_MUX:
    MOV AX, ScrollBuffer.ScrollTimer    ; get scroll counter
    INC AX                              ; increment counter
    AND AX, SCROLL_COUNTER_MODULO_MASK ; wrap by SCMM+1, sets ZF
    MOV ScrollBuffer.ScrollTimer, AX    ; update scroll counter
    
     JZ INCREMENT_SCROLL_BASE            ; time to scroll forward a character
     JNZ NON_SCROLLING_DISPLAY_MUX       ; not scrolling, just standard mux

    INCREMENT_SCROLL_BASE:
    MOV DI, ScrollBuffer.BasePos    ; Get BasePos to scroll from in DI
    INC DI                          ; increment BasePos (scroll forward 1 char)
    AND DI, SCROLL_MODULUS_MASK ; wrap BasePos
    MOV ScrollBuffer.BasePos, DI    ; update BasePos
                    
    JMP OUT_SEGPATTERN     ; Our next segment pattern to display is now exactly 
                           ; ScrollBuffer.LEDSegmentPattern[DI].
                           ; No further processing needed.
    
    NON_SCROLLING_DISPLAY_MUX:
    MOV DI, ScrollBuffer.RelativePos  ; Gets the RelativePosition in DI.
    INC DI                            ; Increments Rel Pos
    AND DI, DISPLAY_MODULUS_MASK      ; Wraps Rel Pos
    MOV ScrollBuffer.RelativePos, DI  ; Stores new RelativePosition
    
    MOV AX, ScrollBuffer.BasePos      ; Gets untouched BasePos in AX
    ADD DI, AX                        ; RelPos + BasePos MOD BufferLength is 
    AND DI, SCROLL_MODULUS_MASK       ; absolute position in buffer.
    
        ; Now DI has the offset (relative to ScrollBuffer.LEDSegmentPatterns)
        ; of the next segment pattern we want to display.
        ; No further processing required.
    JMP OUT_SEGPATTERN

    
    OUT_SEGPATTERN:                     
    MOV DX, ScrollBuffer.RelativePos            ; BL contains IO address
                                                ; in 0-255. Hence BL sufficient
                                                ; see Limitations                                            

    MOV AL, ScrollBuffer.LEDSegmentPatterns[DI]  ; get current segPattern
    OUT DX, AL                                  ; drive display.

    RET
Display_Mux_7Segment ENDP
      
        
; Functional Spec: DisplayInit
; Description:
; This function initializes ScrollBuffer with nulls and sets its position state
; Chip selects for Display IO are set in HW4MainLoop because, in general,
; the setting of MPCS and PACS affects more than just the LED array.
;
; Operation
;   Initialize all ScrollBuffer fields.
;   Loop over ScrollBuffer.LEDSegmentPatterns and set to ASCII_NULL segments.
;
; Arguments: none
;            
; Return Values: None
;
; Local Variables: None with persistent meaning.
;  
; Constants:    
; AmNotScrolling: Scroll flag inactive constant
; SCROLL_COUNTER_RESET: resets ScrollBuffer.ScrollTimer
; EMPTY_STRING_LENGTH: to initialize starting values for positioning
; SCROLL_BUFFER_LENGTH: to know when to terminate our loop zeroing out buffer
;
; Shared Variables: ScrollBuffer, in Data Segment. 
;                   segtable, in Code Segment.
;
; Global Variables: None
;
; Input:            None
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   AX, DX, DI 
; Stack Depth:      0 words
;
; Algorithms:       None
;
; Data Structures:  None
;
; Known Bugs:      None
; Limitations:     None
; Revision History:                 
;   11/15/2014  SSundaresh created
DisplayInit PROC NEAR
            PUBLIC DisplayInit
    MOV AL, AmNotScrolling         ; self evident init statements
    MOV ScrollBuffer.AmScrolling, AL

    MOV AX, SCROLL_COUNTER_RESET
    MOV ScrollBuffer.ScrollTimer, AX

    MOV AX, EMPTY_STRING_LENGTH
    MOV ScrollBuffer.BasePos, AX
    MOV ScrollBuffer.RelativePos, AX
        
    LEA BX, ASCIISegTable
    MOV AL, ASCII_NULL
    XOR AH, AH
    ADD BX, AX
    MOV AL, CS:[BX]      
    
    MOV DI, EMPTY_STRING_LENGTH          
    
    INIT_NULL_FILLER:
    CMP DI, SCROLL_BUFFER_LENGTH        ; start iterating through ScrollBuffer
    JE DISPLAY_INIT_FINISHED            ; done when we've added SBL nulls
    MOV ScrollBuffer.LEDSegmentPatterns[DI], AL
    INC DI
    JMP INIT_NULL_FILLER
    
    DISPLAY_INIT_FINISHED:
    RET 
DisplayInit ENDP

CODE ENDS

; local variables for Display functions
DATA    SEGMENT PUBLIC  'DATA'

ScrollBuffer        SCROLLING_BUFFER   < >
ConvertsBuffer      ConversionsOutputBuffer < >

DATA    ENDS

END