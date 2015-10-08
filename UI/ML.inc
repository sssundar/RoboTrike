; NAME MainLoopConstants.inc
; Description: Includes PCB_Base, PACS and MPCS addresses, and 
; interrupt vector-table setup constants for use with HW4.
;
; Draws much of this from GGeorge's ehdemo.inc, ehdemo.asm.
;
; Revision History:
;  11/15/2014 SSundaresh created.
;  12/17/2014 SSundaresh updated PACSval wait states
;  12/22/2014 SSundaresh renamed

; PCB_BASE finally set for this project
; debuggers sets up LMCS, UMCS.
PCB_BASE EQU 0FF00H     

; Memory Map Control Register Addresses
PACSreg         EQU     PCB_BASE + 000A4H          ;address of PACS register
MPCSreg         EQU     PCB_BASE + 000A8H          ;address of MPCS register

; Control Register Values, chosen using HW1#6 as a guide.
PACSval         EQU     00000H          ;PCS base at 0, 0 wait states
                                        ;0000000000------  starts at address 0
                                        ;----------000---  reserved
                                        ;-------------0--  wait for RDY inputs
                                        ;--------------11  3 wait states
MPCSval         EQU     00183H          ;PCS in I/O space, use PCS5/6, 3 waits 
                                        ;0---------000---  reserved
                                        ;-0000001--------  MCS is 8KB
                                        ;--------1-------  output PCS5/PCS6
                                        ;---------0------  PCS in I/O space
                                        ;-------------0--  wait for RDY inputs
                                        ;--------------11  3 waits

; General Definitions for setting up Interrupt Controller.

FIRST_RESERVED_VEC      EQU     1       ;reserve vectors 1-3
LAST_RESERVED_VEC       EQU     3
NUM_IRQ_VECTORS         EQU     256     ;number of interrupt vectors
