NAME PIOSetup
; Contains:
;  PIOChipSel
;       sets up 8255 assuming PACS/MPCS already set up.
; 		clears output lines
; Revision History:
;  12/22/2014 SSundaresh created

; Contains addressing constants 
$INCLUDE(PIOConst.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP
; Functional Spec: PIOChipSel
;
; Description: Sets up 82C55 parallel IO chip 
; in Mode 0, with all ports as outputs, assuming 
; 8255 addressing already set up via PACS/MPCS in IO cycles.
; Clears all output lines. 
;
; Operation:  The 82C55 control word setup just requires us to 
; address 
; Arguments:        none
; Return values:    none
; Constants:        
; Local Variables:  DX, address of 82C55, AX, control byte to write.
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
;   12/22/2014 SSundaresh  created

PIOChipSel  PROC	NEAR 
			PUBLIC PIOChipSel
		
	MOV DX, CHIP_8255_SETUP_ADDRESS
        MOV AX, CHIP_8255_SETUP_CONTROLBITS 
        OUT DX, AL  	; 8255 Setup, Mode 0, All Ports Output
        
        MOV AL, CHIP_8255_OUTPUT_CLEAR
        
        MOV DX, CHIP_8255_OUTPUTA_ADDRESS  		
        OUT DX, AL      ; clear ports
        
        MOV DX, CHIP_8255_OUTPUTB_ADDRESS
        OUT DX, AL
        
        MOV DX, CHIP_8255_OUTPUTC_ADDRESS
        OUT DX, AL        
		
	RET
PIOChipSel ENDP

CODE ENDS

END