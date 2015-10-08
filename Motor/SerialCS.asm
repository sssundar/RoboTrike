NAME SerialCS
; Contains:
;  SetSerial_ALL
;       sets up TL16C450 Serial IO
;       without handshaking
;       assuming PACS/MPCS already set up.
;       allows interrupts from this source
;       clears out interrupt handler with EOI
;       appropriate to SerialIO
;  Install_SerialEH
;       install serial IO event handler
;       for INT-type interrupt.
;       points to SerialIO_EH in 
;       SerialIO.asm
; Revision History:
;  12/25/2014 SSundaresh created

; Contains addressing constants 
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (SerialCS.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

; a function to be called by the serial interrupt handler
EXTRN SerialIO_EH:NEAR

; Functional Spec: SetSerial_ALL
;
; Note:
; My system will not have a variable
; baud or parity setting. Therefore
; only one setup function is needed.
;
; Description: Sets up TL16C450 serial IO chip
; control registers to enable all interrupts
; except modem status and break conditions.
; Thus, no handshaking. Requires PACS, MPCS
; previously set up. Clears out interrupt 
; controller with an EOI. 
;
; ########################
; Sets INTK interrupt priority and enables 
; interrupt. Priority depends on
; which board this file is being installed
; on. UI Board requires priority lower
; than that of timers, Motor board requires
; timer priority lower than serial interrupts.
; This is because the UI board keypad/display
; can take up to 600us per fairly time insensitive
; interrupt, while serial IO interrupts are happening on
; ms timescales with our current baud settings. 
; Motor interrupts happen on 200us timescales,
; so they take priority over serial io interrupts
; happening on the ms scale.
; #########################
;
; Sets divisor to DLRM_SETTING:DLRL_SETTING
; so baud rate is set, then resets DLAB bit
; to 0 for future addressing assuming
; baud rate will never be changed again.
; 
; 
; Arguments:        none
; Return values:    none
; Constants:   
; from SerialCS.inc
; ADDRESS_LCR ADDRESS_DLRL ADDRESS_DLRM ADDRESS_IER
; LCR_DURING_INITIALIZATION DLRL_SETTING DLRM_SETTING LCR_WHILE_RUNNING
; IER_SETUP
; INTK_CONTROL, INTK_CONTROL_BITS, EOI_INTK
; from GenInts.inc
; EOI
;
; Local Variables:  
; DX, addresses of IO chip, 
; AL, control bytes to write.
; Shared Variables: none
; Global Variables: none
; Registers used:   DX, AX
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   12/25/2014 SSundaresh  created

SetSerial_ALL   PROC NEAR 
			    PUBLIC SetSerial_ALL
    
    ; system RESET expected before this
    ; so all control registers well defined
    ; specifically MCR (no handshaking)

    ; write Line Control Register to
    ; setup word length, number of stop bits
    ; parity, and break signal.
    ; also set DLAB bit high so we can
    ; write baud rate later.
    MOV DX, ADDRESS_LCR
    MOV AL, LCR_DURING_INITIALIZATION   
    OUT DX, AL 
	
    ; write low and high bytes
    ; of the DLR to set 
    ; baud rate.
    MOV DX, ADDRESS_DLRL
    MOV AL, DLRL_SETTING
    OUT DX, AL

    MOV DX, ADDRESS_DLRM
    MOV AL, DLRM_SETTING
    OUT DX, AL

    ; set DLAB bit to 0 so
    ; future RBR and THR read/writes
    ; are properly addressed
    MOV DX, ADDRESS_LCR
    MOV AL, LCR_WHILE_RUNNING   
    OUT DX, AL 

    ; enable specific interrupts
    ; with DLAB set to 0
    MOV DX, ADDRESS_IER
    MOV AL, IER_SETUP
    OUT DX, AL

    ; the priority of this chip's interrupts
    ; depends on the board this file is being
    ; installed on. we also need to enable
    ; interrupts from this source in the 
    ; appropriate interrupt control register.
       
    MOV DX, INTK_CONTROL
    MOV AX, INTK_CONTROL_BITS
    OUT DX, AX

    ; clear out the interrupt handler for this chip's interrupt line
    MOV DX, EOI      ;send an INTK EOI (to clear out controller)
    MOV AX, EOI_INTK
    OUT DX, AX

	RET
SetSerial_ALL ENDP

; Functional Spec: Install_SerialEH
; Description: Set INT_SERIAL vector to point to SerialIO.asm\SerialIO_EH.
; Operation:        Writes the offset and segment of the SerialIO_EH
;                   function in the appropriate location
;                   in the vector table for the INT#K interrupt
;                   corresponding to the serial IO chip.
; Arguments:        none
; Return values:    none
; Constants:        EOI_INTK
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
;   12/22/2014 SSundaresh  repurposed for HW6
;   12/25/2014 SSundaresh  repurposed for HW7
Install_SerialEH PROC NEAR
                PUBLIC Install_SerialEH
        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store SerialIO_EH address in the IVT
                                ;IVT location is exactly 4*EOI_INTK
                                ;if you check Fig2-25 of the manual
        MOV     ES: WORD PTR (4 * EOI_INTK), OFFSET(SerialIO_EH)
        MOV     ES: WORD PTR (4 * EOI_INTK + 2), SEG(SerialIO_EH)
        
        RET
Install_SerialEH ENDP


CODE ENDS

END