NAME PWMTimer
; Contains:
;  Init_PWM_Timer1
; 	 	Does not assume any other timers are used. 
; 	 	Sets up Timer control registers, i.e interrupt register,
; 		max-count, mode, enables Timer1, clears Timer1 interrupt, first pass.
;  Install_PWM_Timer1_EH (installs the Timer1 PWM event handler to point to
; 	 		Motors.asm\PWM_Control
; Revision History:
;  12/22/2014 SSundaresh created
;  12/29/2014 SSundaresh added disable for PWMTimers

; For descriptions of constants and structures see the include file below.
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (GenTimer.inc)    
$INCLUDE (HW4Timer.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

       ASSUME  CS:CGROUP

; a function to be called by the PWM timer interrupt handler
EXTRN PWM_Control:NEAR

; Functional Spec: Init_PWM_Timer1
;
; Description: Initializes Timer1 control registers to setup the
; interrupt frequency and counting mode. 
; Sets Timer1 max count for a 20us timer interrupt. 
; Enables timer and timer interrupts. 
; Sets up Timer interrupt control register for priority, interrupts enabled.
; Sends Timer EOI to clear interrupt controller on first pass.
;
; Operation:  		n/a
; Arguments:        none
; Return values:    none
; Constants:        T1CMPA, T1CNT, T1CON in General_Timers.inc
;                   TIMER_CONTROL in General_Interrupts.inc
;                   BASE_TIMER1, ACTIVE_TIMER1 in HW4Timer.inc
; Local Variables:  none
; Shared Variables: none
; Global Variables: none
; Registers used:   AX, DX
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   10/29/1997 GGeorge     created
;   11/15/2014 SSundaresh  edited reg sizes, constants, scaling factors
;   12/22/2014 SSundaresh  updated for HW6 timer1 PWM setup
Init_PWM_Timer1 PROC NEAR
            	PUBLIC Init_PWM_Timer1
          
    MOV     DX, T1CNT   ;initialize the count register to 0
    XOR     AX, AX      ;clear AX
    OUT     DX, AX      ;clear T1CNT

    MOV     DX, T1CMPA  ;setup max count in appropriate compare register
    MOV     AX, SET_TIMER1_MAXCOUNT
    OUT     DX, AX

    MOV     DX, T1CON    ;setup the control register    
    MOV     AX, ACTIVE_TIMER1 ; continuous count up to CMPA, interrupt, enabled
    OUT     DX, AX

                              ;initialize interrupt controller for alltimers
    MOV     DX, TIMER_CONTROL ;setup the interrupt control register
    MOV     AX, TIMERS_INTERRUPT_CONTROL_REG_VALUE 
                            ; priority set, interrupts enabled
    OUT     DX, AX

    MOV     DX, EOI      ;send a timer EOI (to clear out controller)
    MOV     AX, EOI_TIMER
    OUT     DX, AX
    
    RET
Init_PWM_Timer1 ENDP

; Functional Spec: Disable_PWM_Timer1
;
; Description: Disables Timer1 interrupts.
;
; Operation:        n/a
; Arguments:        none
; Return values:    none
; Constants:        T1CMPA, T1CNT, T1CON in General_Timers.inc
;                   TIMER_CONTROL in General_Interrupts.inc
;                   BASE_TIMER1, ACTIVE_TIMER1 in HW4Timer.inc
; Local Variables:  none
; Shared Variables: none
; Global Variables: none
; Registers used:   AX, DX
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   12/29/2014 SSundaresh  wrote for HW9 EnqueueEvent CError handling
Disable_PWM_Timer1 PROC NEAR
                PUBLIC Disable_PWM_Timer1
                  
    MOV     DX, T1CON    ;setup the control register    
    MOV     AX, BASE_TIMER1 ; interrupts disabled for Timer1
    OUT     DX, AX

    MOV     DX, EOI      ;send a timer EOI (to clear out controller)
    MOV     AX, EOI_TIMER
    OUT     DX, AX
    
    RET
Disable_PWM_Timer1 ENDP	

	

; Functional Spec: Install_PWM_Timer1_EH
; Description: Set Timer1 vector to point to Motors.asm\PWM_Control label.
; Operation:    	Writes the offset and segment of the PWM_Control
; 					function in Motors.asm in the appropriate location
; 					in the vector table for the Timer1 interrupt.   
; Arguments:        none
; Return values:    none
; Constants:        TIMER1VECTOR
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
; 	12/22/2014 SSundaresh  repurposed for HW6
Install_PWM_Timer1_EH PROC NEAR
                PUBLIC Install_PWM_Timer1_EH
        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store PWM_Control address in the IVT
        MOV     ES: WORD PTR (4 * TIMER1VECTOR), OFFSET(PWM_Control)
        MOV     ES: WORD PTR (4 * TIMER1VECTOR + 2), SEG(PWM_Control)
        
        RET
Install_PWM_Timer1_EH ENDP
		

CODE ENDS
        
END