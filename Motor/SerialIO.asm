NAME SerialIO
; Contains:
; SerialGetChar
;   event handler for serial read events
; SerialTX
;   event handler for serial transmission events
; SerialError
;   event handler for serial error events.
; SerialPutChar
;   buffered interface hiding interrupt-driven mechanics of serial transmission 
; Serial_TX_Queue_Init
;   initializes the transmission queue mutated in SerialPutChar
; LoopTillSerialPut    
;   helper that loops calling serialputchar till carry flag reset.
; Limitations: 2400baud required for correct operation. Timing matters
; when running on Motor board with PWM interrupts and known
; 10Hz debounce/keypad repeat rate.
;
; Revision History:
;  12/25/2014 SSundaresh created spec, wrote outline, pseudocode.
;  12/26/2014 SSundaresh addressed SerialPutChar critical code, wrote code.
;  12/29/2014 SSundaresh updated error values/IDs and shifted constants to 
;   more general files. added error value mapping function. added other
;   helper functions.

; Contains addressing constants 
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (SerialCS.inc)
$INCLUDE (queues.inc)

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP

; queue creators, mutators from HW3 for transmission queue 
; EnqueueEvent for serial rx queue.
EXTRN QueueInit:NEAR
EXTRN QueueEmpty:NEAR, QueueFull:NEAR
EXTRN Dequeue:NEAR, Enqueue:NEAR
EXTRN EnqueueEvent:NEAR

; from errors.asm, map LSR error bits to specified error
; ASCII identifiers, on some priority.
EXTRN GET_SERIAL_ERROR_VALUE:NEAR

; Functional Spec: SerialError 
;
; Description: This function is called by the serial event handler, 
; or by SerialPutChar if said function discovers that while trying to
; read the LSR to jump start the Transmission queue it has accidentally 
; wiped a character-received-in-error signal. 
;
; It takes the LSR value last read in AL, so the error interrupt
; is guaranteed cleared. Timing issues (does running this force us to
; miss other interrupts entirely from the SIO chip) are discussed 
; in known-bugs and limitations and will be addressed by
; slowing down the SIO and keypad sampling/baud rates.
;
; Given the LSR value it finds which error flags are up,
; and combines them (they are uniquely positioned) to generate
; a complete serial read error report in one character.
; This is output with an ID SERIAL_ERROR_EVENT_ID using
; EnqueueEvent.
;
; If no error flags were up, the function returns without
; doing anything.
;
; Operation:        Checks comprehensive error bit mask against 
; actual value of LSR and simply uses the result as the event value
; for a SERIAL_ERROR_EVENT_ID call to EnqueueEvent.
; All further error handling is then done by whatever processes the
; event queue.
; Arguments:        AL, LSR value last read by caller
; Return values:    none
; Constants: 
; SERIAL_ERROR_EVENT_ID
; LSR_OVERRUNER EQU 00000010B mask for overrun error bit
; LSR_PARITYERR EQU 00000100B and so on
; LSR_FRAMINGER EQU 00001000B
; LSR_BREAKERRO EQU 00010000B
; LSR_COMPERROR EQU 00011110B is a mask that when anded gives us the entirety
; Local Variables:  
;                   AL, LSR value 
;                   AH, LSR error code masks                 
; Shared Variables: none
; Global Variables: none
; Registers used:   AX
; Stack depth:      0 words
; Input:            error flag from serial IO chip LSR register
; Output:           none
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       see file header
; Limitations:      see file header
; Revision History:
;   12/25/2014 SSundaresh  created
;   12/29/2014 SSundaresh  mapped lsr error code to something
;                           human readable.
SerialError   PROC NEAR 
                PUBLIC SerialError
    MOV AH, LSR_COMPERROR 
    AND AL, AH                      ; get the error bit pattern via mask
    CMP AL, 0                       ; if no errors masked in    
    JE NO_SERIAL_ERRORS_TO_HANDLE   ; don't do anything
    
    CALL GET_SERIAL_ERROR_VALUE     ; map lsr error bits to prespecified 
                                    ; ASCII identifier. returns in AL
    MOV AH, SERIAL_ERROR_EVENT_ID   ; ID is error_event    
    CALL EnqueueEvent               ; enqueue this event           
        
                                    ; the next value read from the RBR
                                    ; might be trash (frame/parity)
                                    ; or ok (overrun)
                                    ; let the error handlers deal 
                                    ; with it in hw9

    NO_SERIAL_ERRORS_TO_HANDLE:
    RET
SerialError ENDP


; Functional Spec: SerialTX
;
; Description: Called by either SerialPutChar, if the SerialTX Queue is
; empty and no THRE interrupt is in the works, or by the interrupt
; handler if a Tx interrupt is called.
; Checks if the Tx byte queue is empty. If it isn't, dequeues a character,
; writes it to the THR. This is not critical as we cannot be interrupted
; here by anyone else who writes the THR, by construction of SerialPutChar.
; If the queue was empty, just exits. It's now the job of the next
; person to call SerialPutChar to restart the interrupt process.
; Operation:  Check if TXQueue empty so call to DeQueue doesn't hang. 
; If empty, exit. If not empty, call Dequeue and load the result into the
; Transmitter holding register of the Serial IO chip.
; Arguments:        none
; Return values:    none
; Constants:        ADDRESS_THR
; Local Variables:  DX, IO address
;                   AL, value to write to THR, dequeued.
; Shared Variables: TXQueue
; Global Variables: none
; Registers used:   AX, DX
; Stack depth:      0 words
; Input:            none
; Output:           a byte dequeued from TXQueue written to serialIO THR for
;                   eventual serial transmission
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       see file header
; Limitations:      see file header
; Revision History:
;   12/26/2014 SSundaresh  created
; PseudoCode
;    check queueempty
;    if not empty
;        dequeue
;        write THR      not critical as cannot be interrupted by myself.           
;    if empty just exit
SerialTX   PROC NEAR 
                PUBLIC SerialTX
    MOV SI, offset(TXQueue)             ; DS:SI is now queue address    
    CALL QueueEmpty
                                        ; ZF set means queue empty.
    JZ SerialTX_TERMINATES              ; in which case we just exit.
                                        ; otherwise we load the next 
                                        ; character to be Txd into the THR

    CALL Dequeue                        ; AL now has the value to be Tx'd   
                                        ; as ours is a byte queue
    MOV DX, ADDRESS_THR
    OUT DX, AL

    SerialTX_TERMINATES:
    RET
SerialTX ENDP


; Functional Spec: SerialGetChar
;
; Description:  Called by the serial event handler when
; a character received interrupt is the highest priority.
; Reads RBR register, enqueues result as a character received event.
; See known bugs in file header - with special attention to timing,
; it is possible to make sure this code as is does not miss Rx interrupts.
; There is no need for this function to save register state.
; Operation:  Read RBR, enqueue result with EnqueueEvent, SERIAL_CHAR_EVENT_ID,
; and the character in the RBR as the event value.
; 
; See known bugs. This code cannot be critical as long as Keypad event handler
; and Serial event handler cannot interrupt each other, as they are the
; only two who add to the EventBuf accessed by EnqueueEvent. Queue
; functions are not safe if they can be Enqueued by different interrupts that
; can nest each other.
; Arguments:        none
; Return values:    none
; Constants:        SERIAL_CHAR_EVENT_ID
; Local Variables:  DX, SerialIO chip address
;                   AH, SERIAL_CHAR_EVENT_ID
;                   AL, character read
; Shared Variables: none
; Global Variables: none
; Registers used:   DX, AX
; Stack depth:      0 words
; Input:            a Serial IO RBR character
; Output:           none
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       see file header
; Limitations:      see file header
; Revision History:
;   12/26/2014 SSundaresh  created
; Pseudocode
;   read RBR
;   write to event queue using EnqueueEvent
; return
SerialGetChar   PROC NEAR 
                PUBLIC SerialGetChar
    MOV DX, ADDRESS_RBR
    IN AL, DX           ; read RBR Rx character into AL
    MOV AH, SERIAL_CHAR_EVENT_ID    
    CALL EnqueueEvent                          
    RET
SerialGetChar ENDP

; Functional Spec: Serial_TX_Queue_Init
; Description:      Initializes transmission buffer. 
; Operation:        n/a
; Shared Variables: TXQueue
; Constants:        BYTE_QUEUE_LEN
; Registers used:   see Queues\QueueInit
; Revision History:
;   12/26/2014 SSundaresh  created
Serial_TX_Queue_Init   PROC NEAR 
                PUBLIC Serial_TX_Queue_Init
    MOV SI, offset(TXQueue)         ; SI holds queue address as DS:SI
    MOV AX, BYTE_QUEUE_LEN
                                    ; length is BYTE_QUEUE_LEN
    MOV BL, 0                       ; element size bytes
    CALL QueueInit
    RET
Serial_TX_Queue_Init ENDP


; Functional Spec: SerialIO_EH
; Description:
; Allows PWM Timer nesting, only on the Motor board. A consequence:
; after the EOI even serial interrupts can nest on the Motor board
; so we might not get to an IRET for a while -
; so if serial interrupts happen too quickly you can get
; a stack overflow.

; checks the interrupt type
; by reading the IIR from the SIO chip. Depending on the priority level,
; either calls (1) SerialError, (2) SerialGetChar, or (3) SerialTX.

; Operation
; Saves register state. On motor board, enables all other maskable interrupts,
; using prior knowledge that we will never enter this event handler with 
; maskable interrupts disabled.

; For Motor code, PWM timer will be priority 0 and SIO priority 6 as default.
; so timer interrupts there will nest, with minimal delay, 
; <5us error, or <0.001 duty cycle error, but SIO nesting not possible.

; Given our otherwise default priority scheme, 
; note this means DMA interrupts can nest, too. But it doesn't look
; like we have allowed those given our schematic.

; For KP/display code, we will not allow nesting. The code below
; must be changed (remove STI). We then just need to 
; carefully choose timing for SIO and KP and DisplayMux
; to make sure we can't lose SIO interrupts.

; The IIR tells us the interrupt priority, which is either due to 
; a serial Rx error, received character, or transmitted character.
; We identify the highest priority cause of the
; interrupt, then call the appropriate event handler. 

; It is possible for multiple interrupt types to be in play at once,
; but just by reading the IIR we can't wipe the others. They'll 
; stick around, and once we acknowledge/clear the highest priority
; bit we'll see them ping us.

; For example, suppose we see an error - we read the LSR.
; This wipes all error bits from the LSR, but we were there to detect
; an error. We've already saved them. The Tx and Rx interrupts, if applicable,
; are still in play. 
; I'm assuming here that errors don't also yield Rx interrupts, but even if 
; they do, I've planned to make sure it doesn't matter.

; Suppose instead we see an Rx interrupt and Tx is done. But I suspect
; since the Tx wasn't the source of the the interrupt, reading the IIR 
; won't clear the Tx flag there. So we read the RBR to deal with the Rx 
; interrupt, then come back to the Tx when it raises its interrupt.

; Suppose finally we see only a Tx interrupt. Then reading the IIR 
; clears the interrupt flag, but we realize we're supposed to deal with a
; Tx interrupt, so no harm done!

; We end by sending an EOI appropriate to EOI_INTK and we're done!

; Arguments:        none
; Return values:    none
; Constants:        IO_Transmitted_Interrupt
;                   IO_ERROR_Interrupt
;                   IO_Received_Interrupt 
;                   ADDRESS_LSR
;                   ADDRESS_IIR                from SerialCS.inc
; Local Variables:  DX, IO addressing
;                   AL, IO or control register read
; Shared Variables: none
; Global Variables: none
; Registers used:   all, flags
; Stack depth:      8 words
; Error handling:   none
; Input:            none (children functions do have, though)
; Output:           none (children functions do have, though)
; Algorithms:       none
; Data structures:  none
; Known bugs:       see file header
; Limitations:      see file header
; Revision History:
;   12/26/2014 SSundaresh  created
;   12/26/2014 SSundaresh  was seeing only first char on hyper
;                          txqueue filled up - wasn't sending EOI
; PseudoCode
;    selectively allow Motor PWM nesting if applicable.
;    check interrupt type by reading IIR 
;      by priority call          
;        1 SerialError
;        2 SerialGetChar
;        3 SerialTX
;      send EOI for EOI_INTK
SerialIO_EH PROC NEAR
            PUBLIC SerialIO_EH	
    PUSHA            
    
                                ; ALLOW NESTING on MOTOR BOARD
    STI                         ; normally, this is not ok, 
                                ; since interrupts might have been
                                ; off for a reason. for our project, though,
                                ; I know I will never turn interrupts
                                ; off otherwise, only mask them. So this STI
                                ; should cause no issue.
                                ; On UI Board remove this. KP 
                                ; cannot interrupt this function.
    
                                
    MOV DX, ADDRESS_IIR
    IN AL, DX                           ; read IIR byte
                               
    CMP AL, IO_ERROR_Interrupt          ; find why we're here
    JE HANDLE_ERROR

    CMP AL, IO_Received_Interrupt
    JE HANDLE_RX

    CMP AL, IO_Transmitted_Interrupt
    JE HANDLE_TX
    JMP TERMINATE_IO_HANDLER            ; if not one of those 3, not handled.


    HANDLE_ERROR:   
                                   
    MOV DX, ADDRESS_LSR                 ; get LSR, call SerialError to
    IN AL, DX                           ; to fully reset interrupts and
                                        ; report error.
    CALL SerialError                    ; error handler       
    
    JMP TERMINATE_IO_HANDLER

    HANDLE_RX:
    CALL SerialGetChar                  ; rx char handler
    JMP TERMINATE_IO_HANDLER    

    HANDLE_TX:
    CALL SerialTX                       ; tx char complete handler
    JMP TERMINATE_IO_HANDLER

    TERMINATE_IO_HANDLER:
    
    MOV DX, EOI      ;send an INTK EOI (to clear out controller)
    MOV AX, EOI_INTK
    OUT DX, AX    
                                ; can be interrupted here
                                ; even by serial io
                                ; possible stack overflow
                                ; if rx/tx interrupts
                                ; too fast.                                           
    POPA       	
    IRET                        ; restores Processor Status Word
SerialIO_EH ENDP

; Functional Spec: SerialPutChar
;
; Description: 
; The function outputs the passed character (c) to the serial channel. 
; It returns with the carry flag reset if the character has been "output" 
; (put in the channel’s queue, not necessarily sent over the serial channel) 
; and set otherwise (transmit queue is full). 
; The character (c) is passed by value in AL.

; Operation: 
; This function is tricky because we want TxQueue processing to be
; interrupt-driven.
;
; Say you initially call this with the TxQueue empty for all previous time.
; We're not receiving any THRE interrupts as we haven't transmitted
; anything. So it's our job to start the ball rolling, not only adding a
; character to the queue, but also calling SerialTX.
; This will take anywhere from 100-1500us to complete once it is called (we 
; want PWMTimer interrupts to interrupt us if we're on the Motor Board,
; so the duty cycle isn't messed up, and until we say otherwise,
; on the UI board or the motor board, we can be interrupted by 
; serial IO interrupts as well, and calls to queue functions
; are expensive).
;
; Then we'll finally receive the interrupt saying our character was
; transmitted, and we'll use that interrupt to process the next 
; character in the queue in an interrupt-driven way.

; Now.. here's the rub. Say we have been doing this a while.
; Now we run SerialPutChar, and check our Queue, and it's full. 
; Well, as far as we know, since between the time we make that 
; call and we do anything else, we could have been interrupted
; by a call to SerialTX that dequeued from our TXQueue. But no matter,
; we just respond with the carry flag set (rotate an appropriate constant),
; and the calling function will try again!

; But what if the queue reads not full. If it has items in it,
; we know we're safe, we know a future SerialTX interrupt will handle
; whatever we place in the queue.

; But what if the queue is empty? Maybe it was just emptied,
; and there's a character in the THR. So if we add to the queue, then call
; SerialTX, we might accidentally overwrite that character! 
; So we need to check the LSR and make sure there's nothing in the THR
; before we write. Only, now, we've erased any pending Rx errors,
; and we've also no idea how many interrupts there were from serial IO
; between all our checks and actions. So sure, we might think there's
; a character in the THR but that interrupt could be long gone by the time
; we actually enqueue something. Which means.. our interrupt-driven
; TXqueue just died.

; So we have to turn off interrupts related to SerialIO before 
; we proceed in this case (queue not full). This requires that we adjusted our
; baud rates and interrupt rates and debounce rates to make sure
; we can't miss Rx interrupts (overruns) by doing so.

; With serial interrupts disabled, we know the TXqueue is not
; being dequeued anymore. So if the queue is not empty, 
; we can add our character and leave. Some interrupt will deal with it.
;
; If the queue is empty, we check the LSR - if the THRE is flagged,
; we can't necessarily expect a Tx interrupt anytime soon, so we should
; call SerialTx ourselves. Even if we had a pending Tx interrupt 
; then our call to SerialTX will wipe it, but generate a new one later.

; However, by checking the LSR we just wiped any error flags, so we 
; also need to call SerialError to make sure errors are properly 
; handled, because we'll never see that interrupt otherwise.

; So our operation is, check if the TXQueue is full. If it is, rotate
; a CARRY_FLAG_CONSTANT once left logically to generate a carry. 

; If it isn't, disallow serial interrupts (timer interrupts as well
; on UI Board, but not on Motor board).
; Specifically, since we've set serial interrupts
; on both boards to have Priority 6, and Timer interrupts on UI board
; to have priority 7, and timer interrupts on the motor board
; to have priority 1, we can just mask interrupts below priority 5.

; Check if the TXQueue is empty.
; Enqueue your character. 
; If not previously empty, just exit
; If it was previously empty, get the LSR of the SIO Chip in AL.
; Call SerialError just in case. 
; If the THRE bit of the LSR is set, call SerialTX. 
; If not, just exit.
;
; Before these exits - reset carry flag,
; and re-allow serial interrupts by setting the PRIMSK to
; 7 again. 
;
; Arguments:        character by value in AL
; Return values:    none
; Constants:        CARRY_FLAG_CONSTANT
;                   PRIORITY_MASK, PRIORITY5, PRIORITY7
;                   ADDRESS_LSR, LSR_TXHREMPTY
; Local Variables:  DX, IO/interrupt control register addressing
;                   AL, LSR register value
;                   SI, queue offset in DS
; Shared Variables: TXQueue
; Global Variables: none
; Registers used:   
; Stack depth:      0 words
; Input:            see Output
; Output:           none directly, see SerialError and SerialTX
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   12/26/2014 SSundaresh  created

; PseudoCode
; check if the TXQueue is full. If it is, rotate
; a CARRY_FLAG_CONSTANT once left logically to generate a carry. 

; If it isn't, disallow serial interrupts (timer interrupts as well
; on UI Board, but not on Motor board).
; Specifically, since we've set serial interrupts
; on both boards to have Priority 6, and Timer interrupts on UI board
; to have priority 7, and timer interrupts on the motor board
; to have priority 1, we can just mask interrupts below priority 5.

; Check if the TXQueue is empty.
; Enqueue your character. 
; If not previously empty, just reset interrupts and CF, and exit
; If it was previously empty, get the LSR of the SIO Chip in AL.
; Call SerialError just in case. 
; If the THRE bit of the LSR is set, call SerialTX then 
; just reset interrupts and CF, and exit
; If not, just reset interrupts and CF, and exit

SerialPutChar   PROC NEAR 
                PUBLIC SerialPutChar	
    MOV SI, offset(TXQueue)             ; queue in DS:SI
    MOV BL, AL                          ; save character, QueueFull uses AX
            
    CALL QueueFull
    JZ TXQueue_Full                     ; ZF set implies queue full
    JNZ TXQueue_NotFull

    TXQueue_NotFull:
    MOV DX, PRIORITY_MASK
    MOV AX, PRIORITY5
    OUT DX, AX                          ; disable serial interrupts       
    
    
                                        ; is TXQueue empty?
    CALL QueueEmpty                     ; SI still points to TXQueue
                                        ; QueueEmpty only uses AX

    PUSHF                               ; preserve that computation
    MOV AL, BL                          ; character input to enqueue
                                        ; address SI set
    CALL Enqueue                        ; won't hang
    POPF                                ; ZF still result of QueueEmpty

    JZ TXQueue_Empty
    JNZ RESTORE_SIO_INTERRUPTS

    TXQueue_Empty:    
    MOV DX, ADDRESS_LSR
    IN AL, DX                           ; get LSR 
    MOV BL, LSR_TXHREMPTY
    AND BL, AL                          ; get only THRE bit of LSR
    
    PUSH BX                             ; AL has LSR
    CALL SerialError                    ; who knows what EnqueueEvent does
                                        ; to BX
    POP BX 
    CMP BL, 0                           
    JNE ENSURE_TX_INTERRUPT             ; if THRE bit is set
    JE RESTORE_SIO_INTERRUPTS

    ENSURE_TX_INTERRUPT:
    CALL SerialTX
    JMP RESTORE_SIO_INTERRUPTS

    RESTORE_SIO_INTERRUPTS:
    
    MOV DX, PRIORITY_MASK
    MOV AX, PRIORITY7
    OUT DX, AX                          ; enable serial interrupts

    MOV AX, CARRY_FLAG_CONSTANT         ; reset carry flag
    SHR AX, 1
    JMP SerialPutChar_DONE    

    TXQueue_Full:       
    MOV AX, CARRY_FLAG_CONSTANT
    SHL AX, 1                   ; carry flag set
    JMP SerialPutChar_DONE

    SerialPutChar_DONE:    	
    RET
SerialPutChar ENDP


; Helper function, calls SerialPutChar
; with the argument in AL
; till it is actually put in the queue.
; Guaranteed to exit unless the chip
; is broken or something else (like
; stack overflow) goes horribly wrong.
; Not blocking. Only called outside
; interrupts via ProcessEvents, 
; ReportError, and such.
LoopTillSerialPut PROC NEAR
           PUBLIC LoopTillSerialPut 
    
    TRY_AGAIN:
    PUSH AX
    CALL SerialPutChar
    POP AX
    JC TRY_AGAIN

    RET
LoopTillSerialPut ENDP
CODE ENDS

;Shared Variables
DATA    SEGMENT PUBLIC  'DATA'

;  our Transmitter byte queue
TXQueue fixedLengthQueue  < >

DATA    ENDS


END