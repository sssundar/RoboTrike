NAME ML
; Functional Spec, HW9ML, Motor Board
; Description: 
; Should be run before UI board sets itself up.
;  Turns off maskable interrupts.
;  
;  Defines peripheral control block base, sets up PCS memory map,
;  Runs ClearIRQVectors by GGeorge to clear the vector table, replace 
;  all non-reserved vectors with pointers to an illegal EH so we can debug 
;  easier.
; 
;  Sets up and initializes SerialIO, ParallelIO, Motor state,
;  Motor PWM timer, Parser state, and Handler state
;  (including event buffer). Clears out interrupt controller
;  in all relevant cases.
; 
;  Re-enables interrupts and loops calling ProcessEvents
; to process the EventBuffer sequentially.
;
; Revision History:
;  11/15/2014 SSundaresh created
;  12/17/2014 SSundaresh updated for hw5
;  12/22/2014 SSundaresh updated for hw6
;  12/25/2014 SSUndaresh updated for hw7
;  12/27/2014 SSUndaresh updated for hw8
;  12/28/2014 SSundaresh final update, hw9 motors

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP, DS:DATA, SS:STACK

EXTRN ClrIRQVectors:NEAR, InitCS:NEAR

EXTRN ResetParserState:NEAR

EXTRN Handlers_EventQueue_Init:NEAR
EXTRN ProcessEvents:NEAR

EXTRN SetSerial_ALL:NEAR
EXTRN Install_SerialEH:NEAR, Serial_TX_Queue_Init:NEAR

EXTRN   Init_PWM_Timer1:NEAR, Install_PWM_Timer1_EH:NEAR
EXTRN   PIOChipSel:NEAR, MotorsInit:NEAR

EXTRN   SetLaser:NEAR, SetMotorSpeed:NEAR

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

        CALL    Install_PWM_Timer1_EH   ; install PWM Timer EH
        CALL    PIOChipSel              ; parallel io chip setup
        CALL    MotorsInit              ; motor state initialized
        CALL    Init_PWM_Timer1         ; timer initialized, interrupts
                                        ; enabled for timer.

        CALL    Handlers_EventQueue_Init ; handler, event buffer init

        
        CALL ResetParserState       ; set up parser FSM                
        MOV AX, 0                   ; reset motor parameters 
        MOV BX, 0
        CALL SetMotorSpeed  
        MOV AX, 0
        CALL SetLaser      

        STI                             ;and finally allow interrupts.                       
                                     
        
Forever: 
        CALL ProcessEvents              ; process events reported in system
        JMP    Forever                 ;sit in an infinite loop
        HLT                             ;never executed hopefully..

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