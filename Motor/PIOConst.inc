; NAME PIOConst.inc
; Contains 82C55 port B bit mappings to the RoboTrike (which bit controls
; which motor function, laser, etc.) and useful associated masks.
; Also contrains 82C55 chip setup constants such as addresses for our
; target boards. 

; 82C55 addressing
; In our target boards, the 82C55 is accessible via PCS3 chip select.
; This means its address is of the form 0180H, with the A0, A1 bits,
; and the instruction OUT/IN
; determining whether we're setting up the chip or trying to write/read
; data (and from which ports).

; Our goal here is to set up the chip as all outputs, Mode 0.
PCS3_BASE			 EQU 0180H   		
                                        ; PCS3 base address given our PACS/MPCS 
 										; setup in HW6ML.inc
WRITE_CONTROL_WORD 	 EQU 0003H  		
                                        ; A0 = 1, A1 = 1
SELECT_PORTA 		 EQU 0000H 			
                                        ; A0 = 0, A1 = 0
SELECT_PORTB 		 EQU 0001H 			
                                        ; A0 = 1, A1 = 0
SELECT_PORTC 		 EQU 0002H 		    
                                        ; A0 = 0, A1 = 1

CHIP_8255_SETUP_ADDRESS 	EQU PCS3_BASE + WRITE_CONTROL_WORD
CHIP_8255_OUTPUTA_ADDRESS 	EQU PCS3_BASE  + SELECT_PORTA
CHIP_8255_OUTPUTB_ADDRESS 	EQU PCS3_BASE  + SELECT_PORTB
CHIP_8255_OUTPUTC_ADDRESS 	EQU PCS3_BASE  + SELECT_PORTC

CHIP_8255_SETUP_CONTROLBITS	 EQU 0080H  	
                                        ; mode 0, ports A,B,C outputs
CHIP_8255_OUTPUT_CLEAR 		 EQU 00000000B

; 8255 Output Bit Meaning to Position Mapping
; for our target board. The meaning of Motor1,2,3 is discussed in
; Motors.asm. Bits 0-7 of the related control word
; follow, LSB bit 0, MSB bit 7.
PORTB_M1DIR  EQU 00000001B 
                    ; Motor 1 Direction, 0 forward, 1 reverse 
PORTB_M1OOF  EQU 00000010B 
                    ; Motor 1 on/off (0 off, 1 on)
PORTB_M2DIR  EQU 00000100B 
                    ; Motor 2 Direction
PORTB_M2OOF  EQU 00001000B 
                    ; Motor 2 on/off
PORTB_M3DIR  EQU 00010000B 
                    ; Motor 3 Direction
PORTB_M3OOF  EQU 00100000B 
                    ; Motor 3 on/off
PORTB_Bit6   EQU 01000000B
                    ; nothing, keep cleared
PORTB_LASER	 EQU 10000000B
                    ; Laser Control (1 on, 0 off)

								; AND masks to clear out motor on/off bits
MOTOR1_OOF_MASK EQU 11111101B
MOTOR2_OOF_MASK EQU 11110111B
MOTOR3_OOF_MASK EQU 11011111B
							 	
LASER_MASK		EQU 01111111B   
                                ; AND mask to clear out laser on/off bit

