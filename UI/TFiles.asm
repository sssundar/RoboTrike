Name TFiles
; Contains:
;  Timer2Init 
;       sets up timer2 control registers, sets up timer interrupt register,
;       enables timer2 and interrupts from timer 2.
;       clears timer2 interrupt, first pass.
;  InstallTimer2EH (installs the Timer2 event handler to point at TimerEH)
;  Timer2EH (calls DisplayFiles.Display_Mux_7Segment)
; Revision History:
;  11/15/2014 SSundaresh created
;  12/31/2014 SSundaresh updated constants for HW9

; For descriptions of constants and structures see the include file below.
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (GenTimer.inc)    
$INCLUDE (HW4Timer.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

       ASSUME  CS:CGROUP
       
; two functions to be called by the timer2 interrupt event handler, to
; mux the display and scan/debounce the target board keypad.
EXTRN   Display_Mux_7Segment:NEAR 
EXTRN   KeypadEH:NEAR

; Functional Spec: Timer2Init
;
; Description: Initializes timer2 control registers. 
; Sets Timer2 max count for a kHz timer interrupt. 
; Enables timer and interrupts. 
; Sets up Timer interrupt control register for priority, interrupts enabled.
; Sends Timer EOI to clear interrupt controller.
;
; Operation:  Word for word, that above.
; Arguments:        none
; Return values:    none
; Constants:        T2CMPA, T2CNT, T2CON in General_Timers.inc
;                   TIMER_CONTROL in General_Interrupts.inc
;                   BASE_TIMER2, ACTIVE_TIMER2 in HW4TimerConstants.inc
; Local Variables:  none
; Shared Variables: none
; Global Variables: none
; Registers used:   AX
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   10/29/1997 GGeorge     created
;   11/15/2014 SSundaresh  edited reg sizes, constants, scaling factors
Timer2Init PROC NEAR
            PUBLIC Timer2Init
          
    MOV     DX, T2CNT   ;initialize the count register to 0
    XOR     AX, AX      ;clear AX
    OUT     DX, AX      ;clear T2CNT

    MOV     DX, T2CMPA  ;setup max count 
    MOV     AX, SET_TIMER2_MAXCOUNT
    OUT     DX, AX

    MOV     DX, T2CON    ;setup the control register    
    MOV     AX, ACTIVE_TIMER2 ; continuous, interrupts, enabled.
    OUT     DX, AX

                              ;initialize interrupt controller for timers
    MOV     DX, TIMER_CONTROL ;setup the interrupt control register
    MOV     AX, TIMERS_INTERRUPT_CONTROL_REG_VALUE 
                            ; priority set, interrupts enabled
    OUT     DX, AX

    MOV     DX, EOI      ;send a timer EOI (to clear out controller)
    MOV     AX, EOI_TIMER
    OUT     DX, AX
    
    RET
Timer2Init ENDP

        

; Functional Spec: InstallTimer2EH
; Description: Set Timer2 vector to point to TimerEH label.
; Operation:  Trivial.  
; Arguments:        none
; Return values:    none
; Constants:        TIMER2VECTOR
; Local Variables:  none
; Shared Variables: none
; Global Variables: none
; Registers used:   AX, ES
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   01/28/2002 Glen George created ehdemo.asm
;   11/15/2014 SSundaresh  copied InstallEH from ehdemo.asm, used my 
;                          General_Interrupts.inc definitions
InstallTimer2EH PROC NEAR
                PUBLIC InstallTimer2EH
        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector
        MOV     ES: WORD PTR (4 * TIMER2VECTOR), OFFSET(Timer2EH)
        MOV     ES: WORD PTR (4 * TIMER2VECTOR + 2), SEG(Timer2EH)
        
        RET
InstallTimer2EH ENDP


; Functional Spec: Timer2EH
; Description: On Timer2 interrupt, simply calls Display_Mux_7Segment.
; Operation:  
;  Interrupt being called implies flags were already pushed.
;  Push all registers; don't know who we interrupted.
;  Call Display_Mux_7Segment
;  Pop all registers.
;  Send Specific Timer2 EOI
;  IRET to pop flags and return.
; Arguments:        none
; Return values:    none
; Constants:        none
; Local Variables:  none
; Shared Variables: none
; Global Variables: none
; Registers used:   none (from outside world's POV)
; Stack depth:      8 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History: 
;   11/15/2014 SSundaresh created
;   12/17/2014 SSundaresh added keypad EH
Timer2EH PROC NEAR
        PUBLIC Timer2EH
    PUSHA
    CALL Display_Mux_7Segment
    CALL KeypadEH
    
    MOV     DX, EOI        ;send specific EOI to the int controller
    MOV     AX, EOI_TIMER
    OUT     DX, AX

    POPA 
    IRET
Timer2EH ENDP   

CODE ENDS
        
END