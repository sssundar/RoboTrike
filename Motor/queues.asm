NAME    queues

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    queues                                  ;
;                           Collection of Queue Functions                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; queues 
; Description: 
; This file contains the procedures 
;  QueueInit 
;  QueueEmpty
;  QueueFull
;  Dequeue
;  Enqueue
; which initialize an allocated memory array as a queue of internal structure
; described in QueueInit, check if said queue is empty or full, and either
; remove (and return) or add in values from said queue. 
; 
; These, together, implement a FIFO queue. 
;
; The structure of the code that follows assumes that enqueing operations
; are only interrupted by dequeueing operations, and vice versa. 
; In this case the procedures that follow should be free of critical code. 
;
; Revision History: 
; 10/31/2014 Sushant Sundaresh wrote, implemented base specs
; 11/02/2014 Sushant Sundaresh cleaned up specs and comments.



; queues.inc
; Description:
; 	This include file contains a fixedLengthQueue structure definition required
; 	for correct operation of the code that follows.
; Revision History: 
; 10/31/2014 Sushant Sundaresh created
; 11/02/2014 Sushant Sundaresh commented
; 12/29/2014 SSundaresh shifted constants for simplicity
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE(queues.inc) 	


CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'

		ASSUME  CS:CGROUP


; QueueInit
; Functional Specification 
; 
; Description:  
; This function takes the starting address, max length, and element size of 
; a pre-allocated memory array as arguments in registers described below.
; It initializes a fixedLengthQueue structure at the address specified.
; This queue structure has a header of length 8 bytes.
; What follows the header depends on the arguments:
;
; The size, if 0, structures the queue as a byte queue.
; The size, if >0, structures the queue as a word queue (2 bytes/element),
; where any word element written to the queue via Enqueue, defined below, 
; must be passed as [HByte LByte]. This is written to memory as [LByte HByte]
; and is returned when read out by Dequeue, also below, as [HByte LByte]. 
; 
; The length refers to the maximum number of elements that can fit
; in the queue. Thus, if the calling function wishes to fit 3 bytes into
; a byte queue, it must allocate the header (8 bytes) + 4 bytes, as 
; the queue enqueuing and dequeing functions by construction leave one 
; unallocated byte to differentiate the queue's empty and full states.
; It would then pass this function a length of 3. 
; Similarly if the caller wants to fit 3 words into a word queue, 
; 8 (header) + 2(3 words) + 2(1 unallocated word) bytes must be allocated.
; The length would still be passed as 3. 
;
; It is required that the length+1 be a positive power of two: 2, 4, ...
;
; This function does not check the requirements above; there is no error 
; handling. 
;
; Once initialized, the queue structure may be accessed and mutated 
; by the procedures QueueEmpty, QueueFull, Dequeue, and Enqueue, also
; defined in this document.
;
; Operation & Algorithms: 
; See first "Arguments" and "queues.inc--fixedLengthQueue" before this.
;
; The memory directly following SI will be structured as a fixedLengthQueue.
; 
; Please keep in mind, this queue will be circular,
; accessed by relative byte indices, the head and tail pointers RelH and RelT,
; both defined in queues.inc--fixedLengthQueue. 
; It is very important to note that these are not relative element indices!
;
; The condition for the queue being empty will be RelH=RelT.
; 
; Below, we will define fixedLengthQueue.ESize and EffLen so that 
; the condition for the queue being full will be
; RelT + ESize mod EffLen = RelH 
;
; This means that one "element" of the queue will always be empty,
; so to hold L elements, the queue must have capacity for L+1 elements.
;
; Let's begin:
;
; The fixedLengthQueue.ESize is set as the queue element byte size 
; (1 word/element = 2 bytes/element), or 1 byte/element for a byte queue.
; ESize is set as 2 (word) if S != 0, and 1 (byte) if S = 0. 
; 
; The max element length parameter L, 
; is assumed of the form 2^k - 1, for positive integer k.
; Thus L = 1 or 3 or 7 or ...
; 
; Let's see the consequences of this requirement:
;
; The number of bytes required to hold L elements + 1 empty element is
; ESize x (L) + ESize if this is to be a word queue, which equals
; 2     x  L  + 2     or, equivalently,
; 2     x  (2^k - 1) + 2 = 
; 2^(k+1) - 2 + 2 = 
; 2^(k+1)
;
; ESize x (L) + ESize if this is to be a byte queue, which equals
; 1     x  L  + 1     or, equivalently,
; 1     x  (2^k - 1) + 1 = 
; 2^(k) - 1 + 1 = 
; 2^(k)
;
; Therefore, the pre-allocation by the calling function must have
; set aside 2^k or 2^(k+1) bytes + the header bytes.
; Also, the max relative index of our head and tail pointers is 2^k - 1 
; or 2^(k+1) - 1, starting from 0, and as appropriate.
;
; Therefore, to wrap our relative byte index pointers 
; and implement a circular queue, we need only mask the relative pointer 
; with 2^(k)-1 or 2^(k+1)-1, again as appropriate. 
; 
; This mask is what we calculate and store in fixedLengthQueue.EffLen.
; Specifically, we calculate ESize * L + ESize - 1,
; which for a byte queue reduces to 
; EffLen = L
; and which for a word queue reduces to
; EffLen = 2L + 1. 
;
; Finally, 
; RelH is the queue head pointer, defined relative to the 0 element
; of the fuxedLengthQueue.wrappedBuffer, as a relative byte index. 
; The element starting at the head pointer is returned when Enqueue is called.
;
; RelT is the queue tail pointer, defined relative to the 0 element
; of the fixedLengthQueue.wrappedBuffer, as a relative byte index. 
; The element bytes starting at the tail pointer is where a new element is 
; written when enqueue is called.
;
; RelH and RelT are set to 0 (start of the wrappedBuffer) to empty the queue
; before declaring it initialized.

; Arguments: 
; fixedLengthQueue base address A
;		passed in SI by value, queue starts at DS:[SI]
; 		thus, [SI].headerElement accesses the address of the 
; 		fixedLengthQueue header element specified,
; 		and [SI].wrappedBuffer[index] accesses the address of the
; 		queue data element specified.
; fixedLengthQueue maximum length, L
;		passed in AX by value, must be equal 
; 		to (2^k)-1 for some positive integer k
; fixedLengthQueue element size S
;		passed in BL by value. 
;		s==0 initializes a byte queue,
;		s >0 initializes a word queue
;
; Return Values:    None
;
; Local Variables : The only values with persistent meaning are the arguments,
; already described above.
;
; Constants: 		
; fixedLengthQueue STRUCT
; WORD_BYTE_SIZE
; BYTE_BYTE_SIZE	
; all clearly defined in queues.inc
;
; Shared Variables: the queue in SI
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, AX, SI, BX
; Stack Depth:      0 words
;
; Algorithms:       see Operations for consequences.
; 	In this file, we are implementing a circular queue.
;   Below, H = RelH, T = RelT.
;
;   Let us denote this queue intialized as:
; 	_   _   _   _   _   _   _  ...
;   HT
;   This is our empty condition.
;
;   Upon adding an element V0, we require:
; 	V0   _   _   _   _   _   _  ...
;   H    T
;   Where we have added the element, and THEN
;   we have incremented the tail pointer.
;
;   Upon dequeueing the head element, we require:
; 	V0   _   _   _   _   _   _  ...
;        HT
;   Where we have accessed the value at the old head, V0, stored it, 
;   then incremented H. Now, the queue is empty once more.
; 
;   Suppose now we add many elements V, so T wraps around.
; 	V0   V   V   V   V   V   V  ...   V
;   T    H       
;   now we cannot add another element, else T = H and the queue 
;   looks empty. So we say we are full at T + 1 = H, wrapped.
;   Note that the value V0 at T is not 'valid data' anymore. 
;   T points to the next index to be filled;
;   it never points to never-accessed enqueued data.
; 
;   If you continue this reasoning, you'll see there are no edge cases.
;
; Data Structures:  fixedLengthQueue
;					Initializes queue with the internal structure of a 
;					fixedLengthQueue STRUC as defined in queues.inc
;
; Known Bugs:       None
; Limitations:      Only handles queues that hold up to 2^k - 1 elements.
; 					Furthermore, below, the accessors and mutators
; 					act as though WORD_BYTE_SIZE = 2, not variable;
;					but if WORD_BYTE_SIZE > 2, we'd have to significantly
;					restructure the code to use other number representations
;					anyways.		
;				
; Revision History:					
;	10/31/2014 SSundaresh wrote spec, wrote code, tested assembly
; 	11/02/2014 SSundaresh revised specs and comments.
; 
; Pseudocode
; 	IF S == 0
; 	THEN 
;	set queue.elementsize = BYTE_BYTE_SIZE ; byte queue 
;	set queue.effectivelength = L   ; rel. index mask described in operation
;	ELSE 
;	set queue.elementsize = WORD_BYTE_SIZE ; word queue 
;	set queue.effectivelength = 2L+1 ; rel. index mask described in operation
;	ENDIF
;
;	set queue.relativeheadbyteindex = 0
;	set queue.relativetailbyteindex = 0		; empty condition
;	return 							

QueueInit	PROC	NEAR
			PUBLIC	QueueInit						
	CMP BL, 0					; BL > 0 initializes a word queue
	JE 	ASSERT_BYTE_QUEUE		; BL = 0 initializes a byte queue
	
	ASSERT_WORD_QUEUE:
		MOV [SI].ESize, WORD_BYTE_SIZE	; element size is WORD_BYTE_SIZE bytes
		MOV BX, AX 						; calculate 2L + 1
		ADD AX, BX						
		INC AX					        
		MOV [SI].EffLen, AX             ; set EffectiveLength as 2L+1 
		JMP INITIAL_STATE_EMPTY        
								
	ASSERT_BYTE_QUEUE:	                   
		MOV [SI].ESize, BYTE_BYTE_SIZE  ; element size is BYTE_BYTE_SIZE bytes	
		MOV [SI].EffLen, AX				; EffectiveLength as L
		
	INITIAL_STATE_EMPTY:
		MOV [SI].RelH, 0	; relative head byte index = relative tail 
		MOV [SI].RelT, 0	; byte index = 0, so we're at the start of
							; [SI].wrappedBuffer when we next enqueue.
					
	RET
QueueInit	ENDP


; QueueEmpty
; Functional Specification 
; 
; Description:  
; This function takes the starting address of a queue initialized by
; QueueInit (a fixedLengthQueue) and otherwise structurally unmodified.
; The queue is expected to be at [SI].
; It returns the ZF (zero flag) set to 1 if the queue is empty, and set to
; 0 otherwise. 
;
; Operation:
; Check whether the relative head byte index of the fixedLengthQueue passed
; is equal to the relative tail byte index. If so, compare a 0 with a 0
; to set the ZF to 1, and otherwise compare a 1 with a 0 to set the ZF to 0.
;
; Arguments: 
; fixedLengthQueue base address A
;		passed in SI by value, queue starts at DS:[SI]
;
; Return Values:    ZF, set to 0 if not empty, 1 if empty.
;
; Local Variables:  None
;
; Constants: 		
; fixedLengthQueue STRUCT, clearly defined in queues.inc	
;
; Shared Variables: the queue in SI
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, SI, AX
; Stack Depth:      0 words
;
; Algorithms:       See Algorithms of QueueInit.
;
; Data Structures:  assumes fixedLengthQueue structure for input queue
;
; Known Bugs:       None
; Limitations:      None		
;				
; Revision History:					
;	10/31/2014 SSundaresh wrote spec, wrote code, tested assembly
; 	11/02/2014 SSundaresh revised specs and comments.
; 
; Pseudocode
; 	IF queue.RelH = queue.RelT
; 	THEN 
;	set ZF to 1  		; queue is empty
;	ELSE 
;	set ZF to 0  		; queue is not empty
;	ENDIF
;	return 		

QueueEmpty	PROC 	NEAR
			PUBLIC 	QueueEmpty		
	MOV AX, [SI].RelH 				; load queue RelH
	CMP AX, [SI].RelT 				; is RelH equal to RelT?
	JE QUEUE_EMPTY 					; if so, we're empty.
	
	QUEUE_NOT_EMPTY: 				
		MOV AX, 1 					; compare a 1 with a 0 to 
		CMP AX, 0		    		; set the zero flag to 0 (not zero)
		JMP END_QUEUE_EMPTY 		; signifying the queue is not empty
		
	QUEUE_EMPTY:
		MOV AX, 0 					; compare a 0 with a 0 to
		CMP AX, 0 					; set the zero flag to 1 (is zero)
		;JMP END_QUEUE_EMPTY 		; signifying the queue is empty
	
	END_QUEUE_EMPTY:	
	RET 
QueueEmpty	ENDP



; QueueFull
; Functional Specification 
; 
; Description:  
; This function takes the starting address of a queue initialized by
; QueueInit (a fixedLengthQueue) and otherwise structurally unmodified.
; The queue is expected to be at [SI].
; It returns the ZF (zero flag) set to 1 if the queue is full, and set to
; 0 otherwise. 
;
; Operation:
; Check whether the relative tail byte index of the fixedLengthQueue passed
; is pointing at the start of the element just before the element pointed to by
; the relative head byte index, where "just before" includes wrapping
; around the edge of the queue back to the beginning. 
; 
; As described in the Operation of QueueInit,
; Numerically this works out to RelT + ESize mod EffLen = RelH, 
; as RelT and RelH are byte indices, not element indices, 
; and EffLen is the mask constructed to 
; correctly wrap RelT and RelH via the AND operation.
; 
; Thus, we first compute RelT + ESize mod EffLen as
; AND RelT+ESize, EffLen 
; and check whether this equals RelH.
; If it does, we set the ZF (zero flag) to 1 by computing CMP 0,0
; to indicate the queue is full. 
; Otherwise we set the ZF to 0 by computing CMP 1,0 to indicate 
; the queue is not full.
;
; Arguments: 
; fixedLengthQueue base address A
;		passed in SI by value, queue starts at DS:[SI]
;
; Return Values:    ZF, set to 0 if not full, 1 if full.
;
; Local Variables:  None
;
; Constants: 		
; fixedLengthQueue STRUCT, clearly defined in queues.inc	
;
; Shared Variables: the queue in SI
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, SI, AX
; Stack Depth:      0 words
;
; Algorithms:       See Algorithms of QueueInit.
;
; Data Structures:  assumes fixedLengthQueue structure for input queue
;
; Known Bugs:       None
; Limitations:      None		
;				
; Revision History:					
;	10/31/2014 SSundaresh wrote spec, wrote code, tested assembly
; 	11/02/2014 SSundaresh revised specs and comments.
; 
; Pseudocode
; 	IF queue.RelT + 1 MOD EFFLEN = queue.RelT 	; see QueueInit Algorithms
; 	THEN 
;	set ZF to 1  		; queue is empty
;	ELSE 
;	set ZF to 0  		; queue is not empty
;	ENDIF
;	return 

QueueFull	PROC 	NEAR
			PUBLIC 	QueueFull		
	MOV AX, [SI].ESize   			
	CMP AX, 2 						; If ESize = 2, we are a word queue
									; and the wrap modulus is computed
									; differently.
	JE CHECK_FULL_WORD_QUEUE
	
	CHECK_FULL_BYTE_QUEUE:			; This is a byte queue, so 
									; by the Algorithm described in QueueInit,
		MOV AX, [SI].RelT 			; AX contains RelT
		ADD AX, [SI].ESize	 		; AX contains RelT + ESize
		AND AX, [SI].EffLen 		; AX contains RelT + 1 modulo the size of 
									; the queue.wrappedBuffer
									; by construction of EffLen for a bytequeue
		CMP AX, [SI].RelH 			; If this equals RelH, 
		JE QUEUE_FULL 				; the queue is full.
		JNE QUEUE_NOT_FULL
	
	CHECK_FULL_WORD_QUEUE: 			; This is a word queue, so 
		MOV AX, [SI].RelT 			; by the Algorithm defined in QueueInit,
		ADD AX, [SI].ESize	 		; we compute RelT + ESize
		AND AX, [SI].EffLen 		; AX contains RelT + ESize mod the size of 
									; the queue.wrappedBuffer
									; by construction of EffLen for a wordqueue
		CMP AX, [SI].RelH 			; If this equals RelH
		JE QUEUE_FULL 				; We're full.
		JNE QUEUE_NOT_FULL
	
	QUEUE_NOT_FULL:  				; If we're not full,
		MOV AX, 1 					; Compare 1,0 to set the ZF to 0
		CMP AX, 0 					; to indicate we're not full.
		JMP END_QUEUE_FULL
	
	QUEUE_FULL:
		MOV AX, 0 					; otherwise compare 0,0 to set ZF = 1
		CMP AX, 0 					; to indicate we're full.
		
	END_QUEUE_FULL:	
	RET	
QueueFull	ENDP


; Dequeue
; Functional Specification 
; 
; Description:  
; This function takes the starting address of a queue initialized by
; QueueInit (a fixedLengthQueue) and otherwise structurally unmodified.
; The queue is expected to be at [SI].
; It dequeues the datum at the head pointer of the queue, and returns it in
; the register AX or AL, depending on whether the queue is of words or bytes.
; The returned value is identical to that passed to Enqueue when the datum
; was first enqueued (byte order unchanged).
; 
; This function is not critical code as long as the Dequeue function
; cannot be interrupted by another call of the Dequeue function
; on the same queue. 
; 
; This function IS blocking; it does not return a value until it has 
; dequeued something from the queue.

; Operation:
; We check whether the queue at [SI] is empty using QueueEmpty and ZF.
; If it is, we loop until it is not empty.
;
; If it is not empty, there is a datum to be dequeued at the head of the queue.
; We check the element size to make sure we copy the right number of bytes
; as we are trying to dequeue an entire element.
;
; If we have a word queue, we copy 
; set AL = queue.wrappedBuffer[RelH]
; non-mutably calculate RelH+ESize-1 
; There is no loop, as ESize = 2, and RelH+ESize-1 guaranteed not to wrap by 
; construction.
; set AH = queue.wrappedBuffer[RelH+1]
; increment RelH by ESize.
; 
; If we have a byte queue we copy
; AL = queue.wrappedBuffer[RelH]
; and set AH = 0
; then increment RelH by ESize.
;
; This whole time, we could have been interrupted. However, you'll notice we 
; never incremented the head pointer. Therefore, as long as we are only 
; interrupted by an Enqueuing function call, only the tail pointer will move.
; Since in our case Enqueue and Dequeue will be called exclusively by different
; control boards, even if Enqueue calls repeatedly, the worst that can happen 
; is that RelT + 1 = RelH, at which point one board will see Enqueue start 
; blocking. There is no case where Enqueue writes over the value we're 
; looking at, because we haven't touched the head pointer yet.
; 
; If we were allowed to get interrupted by Dequeue calls, 
; then even if we don't change the head pointer, another dequeue call might.
; Then, if we return our value, first, we'll return it out of order,
; and second, when we do increment our head pointer, we'll skip over
; a valid, other index RelH than the one we thought we were skipping!
; Technically the head pointer would still point to valid data, so it's not
; really critical code.. but it is weird.
;
; Alternatively, in the same situation, we might have called "QueueEmpty,"
; which returns False, so we're prepping to dequeue, but then if we got 
; interrupted by a Dequeue call, which did make the queue empty, when we 
; returned to our original call, the sign flags would be reset as if the other
; interrupt had never happened - therefore we'd proceed to dequeue from
; an empty queue! 
;
; So, assuming we cannot get interrupted by Dequeue calls, we do not need
; STI or CLI calls. We can just increment the head pointer last.
;
; This is exactly what we do.
;
; Arguments: 
; fixedLengthQueue base address A
;		passed in SI by value, queue starts at DS:[SI]
;
; Return Values:    ESize bytes at queue.wrappedBuffer[RelH:RelH+ESize-1]
; 					in AX or AL for word/byte queues, respectively,
;					with the order of the bytes in AX 
;					the same as in the datum originally passed to Enqueued
;					that is, with byte order preserved.
;
; Local Variables:  None
;
; Constants: 		
; fixedLengthQueue STRUCT, clearly defined in queues.inc	
;
; Shared Variables: the queue in SI
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None
;
; Registers Used:   flags, SI, AX, BX
; Stack Depth:      0 words
;
; Algorithms:       See Algorithms of QueueInit.
;
; Data Structures:  assumes fixedLengthQueue structure for input queue
;
; Known Bugs:       None
; Limitations:      Requires Dequeue to never be interrupted by Dequeue.
	
;				
; Revision History:					
;	10/31/2014 SSundaresh wrote spec, wrote code, tested assembly
; 	11/02/2014 SSundaresh revised specs and comments.
; 
; Pseudocode
;	CALL QueueEmpty
; 	IF ZF set  
;		go back to start 			;block till not empty, and can dequeue
;   If WORD_QUEUE
;       Store RelH in BX
; 		MOVE queue.buffer[H] to AL
;		Store H+ESize-1 = H+1 in BX 	; haven't changed RelH.
; 		MOVE queue.buffer[H+1] to AH
;		increment RelH by ESize
;   ELSE ; byte queue
;		Store RelH in BX
; 		MOVE queue.buffer[H] to AL
;		SET AH 0
;		increment RelH by ESize
;	ENDIF

Dequeue 	PROC 	NEAR
			PUBLIC 	Dequeue
	DEQUEUE_BLOCKER:				
	CALL QueueEmpty						
	JZ DEQUEUE_BLOCKER					; if ZF 1, empty, so block.
										; otherwise ZF 0, not empty, continue.
	MOV BX, [SI].ESize 					
	CMP BX, 1 							; ESize = 1 implies byte queue.
	JE DEQUEUE_INCREMENT_HEAD_BY_BYTE 
	
	DEQUEUE_INCREMENT_HEAD_BY_WORD:   	; Else we're a word queue.
		MOV BX, [SI].RelH	   			; store RelH so we don't have to change
		MOV AL, [SI].wrappedBuffer[BX]  ; it. Get low byte at RelH in AL.
		INC BX 							; BX = RelH+ESize-1, for a word.
										; no need to wrap by construction.
		MOV AH, [SI].wrappedBuffer[BX]  ; AH stores high byte
										; of the word we're trying to dequeue.
		
		INC BX							; next byte is start of a new element	 
		AND BX, [SI].EffLen 			; wrap BX, the intended updated RelH
		MOV [SI].RelH, BX				; update RelH.		
	RETURN_WORD:		
		JMP END_DEQUEUE 				; AX contains word dequeued, we're done
	
	DEQUEUE_INCREMENT_HEAD_BY_BYTE:
		MOV BX, [SI].RelH	 			; get RelH
		MOV AL, [SI].wrappedBuffer[BX] 	; get AL from head of byte queue.
		  
		INC BX 							; BX is intended update for RelH
		AND BX, [SI].EffLen 			; Mask it to wrap,
		MOV [SI].RelH, BX				; then update RelH.
	RETURN_BYTE:
		AND AH, 0		 				; and clear the high byte just in case
		
	END_DEQUEUE:
	RET
Dequeue		ENDP

; Enqueue
; Functional Specification 
; 
; Description:  
; This function takes the starting address of a queue initialized by
; QueueInit (a fixedLengthQueue) and otherwise structurally unmodified.
; The queue is expected to be at [SI].
; It takes also a byte or word to be enqueued into the queue element pointed 
; at by the relative tail pointer, at AL or AX respectively.
;
; The input byte or word is written into the queue, and the function returns.
; There is no error handling, if you try to input a word into a byte queue,
; only the low byte will be written. 
; If you do not clear your high byte before trying to write a byte 
; into a word queue, you will end up writing AH to the queue as well, 
; whatever it is.
; 
; This function is not critical code as long as the Enqueue function
; cannot be interrupted by another call of the Enqueue function
; on the same queue. The reason for this is identical to that given in 
; Dequeue-Operation, above, and is not repeated.
; 
; This function IS blocking; it does not return a value until it has 
; enqueued something into the queue.
;
; Operation:
; We push AX to the stack, then call QueueFull (which mutates AX).
; We the pop AX, which contains our value to enqueue, and 
; We check whether the queue at [SI] is full using QueueFull and ZF.
; If it is full, ZF = 1, and we loop until it is not full.
;
; If it is not full, there is a place for our datum to be enqueued, 
; at the tail of the queue. 
; We check the element size to make sure we write the right number of bytes
; as we are trying to dequeue an entire element.
;
; If we have a word queue, we save
; RelT to BX
; AL->RelT, AH->RelT+1, where RelT is not mutated
; then update RelT to RelT+ESize, wrapped.
; When Dequeued, the byte order will be restored.
;
; If a byte queue, RelT->BX, AL->RelT without updating RelT, 
; then we update RelT->RelT+ESize, wrapped.
;
; Note that as with Dequeue,assuming we cannot get interrupted by Enqueue 
; calls, we do not need STI or CLI calls. 
; We can just increment the tail pointer last.
;
; This is exactly what we do.
;
; Arguments: 
; fixedLengthQueue base address A
;		passed in SI by value, queue starts at DS:[SI]
; datum to enqueue, AX or AL if word or byte queue, respectively.
;
; Return Values:    None
;
; Local Variables:  None
;
; Constants: 		
; fixedLengthQueue STRUCT, clearly defined in queues.inc	
;
; Shared Variables: the queue in SI
; Global Variables: None
;
; Input:            None 
; Output:           None
;
; Error Handling:   None; see Description, you might not get the result
;					you expect if you write words to byte queues or vice 
;					versa.
;
; Registers Used:   flags, SI, AX, BX
; Stack Depth:      1 word
;
; Algorithms:       See Algorithms of QueueInit.
;
; Data Structures:  assumes fixedLengthQueue structure for input queue
;
; Known Bugs:       None
; Limitations:      Requires Enqueue to never be interrupted by Enqueue.
;					No error handling for invalid byte/word input to 
;					word/byte queue.
;				
; Revision History:					
;	10/31/2014 SSundaresh wrote spec, wrote code, tested assembly
; 	11/02/2014 SSundaresh revised specs and comments.
; 
; Pseudocode
; 	PUSH DATUM IN AX TO STACK
;	CALL QueueFull 	; which mutates AX
;	POP DATUM from STACK
; 	IF ZF set  
;		go back to start 			;block till not full, and can enqueue
;   If WORD_QUEUE
;       Store RelT in BX
; 		WRITE AL to queue.buffer[BX] 
;		Store RelT+ESize-1 = RelT+1 in BX 	; haven't changed RelT
; 		WRITE AH to queue.buffer[BX] 
;		increment BX once more ; BX = RelT + ESize = RelT + 2
;		Update RelT with BX
;   ELSE ; byte queue
;		Store RelT in BX
; 		WRITE AL to queue.buffer[BX] 
;		increment BX by ESize = 1 for byte queue
;		update RelT with BX
;	ENDIF

Enqueue		PROC 	NEAR
			PUBLIC 	Enqueue
	ENQUEUE_BLOCKER:
	PUSH AX						; save datum input 
	CALL QueueFull		 		; as QueueFull mutates AX
	POP AX 						; restore datum input
	JZ ENQUEUE_BLOCKER 			; if ZF = 1 queue is full, block. 
			
	MOV BX, [SI].ESize 			; otherwise check if this is a byte queue
	CMP BX, 1 					
	JE ENQUEUE_ADD_BYTE_AT_TAIL
	
	ENQUEUE_ADD_WORD_AT_TAIL: 	
		MOV BX, [SI].RelT 				; store local copy of RelT 
		MOV [SI].wrappedBuffer[BX], AL 	; write low byte
		INC BX 							; increment local RelT
		MOV [SI].wrappedBuffer[BX], AH 	; write high byte
										; guaranteed not to wrap
		INC BX							; update to start of next word element
		AND BX, [SI].EffLen 			; wrap BX = new RelT
		MOV [SI].RelT, BX 				; update RelT
		JMP END_ENQUEUE
		
	ENQUEUE_ADD_BYTE_AT_TAIL:
		MOV BX, [SI].RelT 				; get local RelT
		MOV [SI].wrappedBuffer[BX], AL  ; write low byte input
		INC BX
		AND BX, [SI].EffLen
		MOV [SI].RelT, BX				; increment local relT to next element,
										; wrap, and update RelT.
		
	END_ENQUEUE:
	RET
Enqueue		ENDP

CODE    ENDS



        END
