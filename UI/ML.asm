NAME ML
; Functional Spec, HW9ML, UI Board
; Description: 
; Should be run after motor board sets itself up.
;  Turns off maskable interrupts.
;  
;  Requires debugger to set up LCS/UCS/MCS. 
; 
;  Defines peripheral control block base, sets up PCS memory map,
;  Runs ClearIRQVectors by GGeorge to clear the vector table, replace 
;  all non-reserved vectors with pointers to an illegal EH so we can debug 
;  easier.
;
;  Sets up timer interrupts, serial IO, and keypad/display, parser. Sets
;  up event processing. 
; 
; Turns on maskable interrupts. 
; Loops calling ProcessEvents indefinitely.
;
; Revision History:
;  11/15/2014 SSundaresh created
;  12/17/2014 SSundaresh updated for hw5
;  12/22/2014 SSundaresh updated for hw6
;  12/25/2014 SSUndaresh updated for hw7
;  12/27/2014 SSUndaresh updated for hw8
;  12/31/2014 SSundaresh final update, hw9 UI
;  1/1/2015 SSundaresh final update, UI, motors: change
; groupings, leave stack on its own - no more stack overflow
; like issues then.

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP, DS:DGROUP, SS:STACK

EXTRN ClrIRQVectors:NEAR, InitCS:NEAR
EXTRN Timer2Init:NEAR, InstallTimer2EH:NEAR
EXTRN DisplayInit:NEAR
EXTRN KPInit:NEAR
EXTRN SetSerial_ALL:NEAR, Install_SerialEH:NEAR, Serial_TX_Queue_Init:NEAR
EXTRN ResetParserState:NEAR
EXTRN Handlers_EventQueue_Init:NEAR, ProcessEvents:NEAR
START:

MAIN:
		CLI 							; disallow interrupts

        MOV     AX, STACK               ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(TopOfStack)

        MOV     AX, DATA                ;initialize the data segment
        MOV     DS, AX


        CALL    InitCS                  ;initialize the 80188 chip selects
                                        ;   assumes LCS and UCS already setup

        CALL    ClrIRQVectors        ;clear (initialize) interrupt vector table

        CALL    Install_SerialEH    ; install Serial IO interrupt handler
        CALL    SetSerial_ALL       ; initializer serial IO chip
        CALL    Serial_TX_Queue_Init ; initialize transmission queue

        CALL    InstallTimer2EH     ; install KP debounce/display mux timer 
        CALL    DisplayInit         ; initialize display buffers
        CALL    KPInit              ; initialize keypad scanning/debouncing

        CALL    ResetParserState    ; reset parser state 
        CALL    Handlers_EventQueue_Init    ; initialize EventBuffer

        CALL    Timer2Init          ; start KP db/ display mux timer.        
      
        STI                             ;and finally allow interrupts.                       
         
		
Forever: 	
        CALL ProcessEvents
        JMP    Forever                 ;sit in an infinite loop 
                                       ; calling ProcessEvents
        HLT                            ;never executed (hopefully)        

CODE ENDS

;the data segment
DATA    SEGMENT PUBLIC  'DATA'

DATA    ENDS

;the stack
STACK           SEGMENT STACK  'STACK'
                DB      80 DUP ('Stack ')       ;240 words
TopOfStack      LABEL   WORD
STACK           ENDS

END     START