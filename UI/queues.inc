; queues.inc
; Description:
; 	This include file contains a fixedLengthQueue structure definition required
; 	for correct operation of the code that follows.
; Revision History: 
; 10/31/2014 Sushant Sundaresh created
; 11/02/2014 Sushant Sundaresh commented
; 12/25/2014 SSundaresh removed QUEUE_SIZE to SerialIO.inc and
; added .inc dependency to queues.asm

; Constants

BYTE_BYTE_SIZE EQU 1 ; a byte is one byte
WORD_BYTE_SIZE EQU 2 ; a word is two bytes

; Structures

fixedLengthQueue	STRUC	
;HEADER
	ESize			DW ?					
  
	EffLen 			DW ?		
   
	RelH	 		DW ?	
   
	RelT	 		DW ?	
   
   ;ESize is the queue element size (2 bytes/element, or 1 byte/element)
   ;EffLen is the effective byte length required of the wrappedBuffer.

   ;When this queue is initialized, by QueueInit, in queues.asm,
   ;it will be passed a length parameter L, defining the maximum number
   ;of elements the queue must hold. 
   ;EffLen is calculated as ESize x (L) + 1 if this is to be a word queue,
   ;and 					ESize x	(L) 	if this is to be a byte queue.
   ;This is because ESize x L is the number of bytes required to hold the 
   ;L elements, and we require one "empty" element in the queue at all times
   ;to distinguish empty and full states.
   ;Since EffLen is required to be of the form 2^k-1, for proper operation
   ;of the queue functions in queues.asm, if ESize = 2 for a word queue,
   ;we can see that ESize*L = 2^(k+1) - 2, so we add one. 

   ;RelH is the queue head pointer, defined relative to the 0 element
   ;of the wrappedBuffer, i.e. the address fixedLengthQueue.wrappedBuffer
   ;The value pointed to by the head pointer is returned when Enqueue is called
   ;in the associated queues.asm file. 

   ;RelT is the queue tail pointer, defined relative to the 0 element
   ;of the wrappedBuffer, i.e. the address fixedLengthQueue.wrappedBuffer
   ;The value pointed to by the tail pointer is where a new element is 
   ;added when enqueue is called.

;QUEUE QUEUE_SIZE defined in SerialIO.inc, a dependency in HW7
	wrappedBuffer 	DB QUEUE_SIZE DUP (?)
   
   ;wrappedBuffer is an array of physical byte length QUEUE_SIZE which
   ;holds the values that are enqueued and can be dequeued from this queue.

fixedLengthQueue 	ENDS



