NAME ChipSel

;Description: Chip selects for homeworks 5- onward. 
;Last Revised: 12/17/2014 SSundaresh branched from HW4ML.asm

; PACS, MPCS register values defined
$INCLUDE (ML.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'
	ASSUME CS: CGROUP

; InitCS
;
; Description:       Initialize the Peripheral Chip Selects on the 80188.
;
; Operation:         Write the initial values to the PACS and MPCS registers.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Revision History
;  12/05/2000 GGeorge created
;  11/15/2014 SSundaresh updated regs from AL to AX.

InitCS  PROC    NEAR
        PUBLIC InitCS


        MOV     DX, PACSreg     ;setup to write to PACS register
        MOV     AX, PACSval
        OUT     DX, AX        ;write PACSval to PACS (base at 0, 3 wait states)

        MOV     DX, MPCSreg     ;setup to write to MPCS register
        MOV     AX, MPCSval
        OUT     DX, AX        ;write MPCSval to MPCS (I/O space, 3 wait states)


        RET                     ;done so return


InitCS  ENDP

CODE ENDS

END