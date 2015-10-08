; NAME SerialCS
; Contains interrupt control bits for serial IO.
; Contains chip select addressing constants for 
; setup and control of the TL16C450 SerialIO chip
; Last Revision: 12/25/2014 SSundaresh created

; Interrupt vector for SerialIO
; requires GenInts.inc preloaded
EOI_INTK EQU EOI_INT2

; Serial Interrupt priority for this board
INTK_PRIORITY EQU PRIORITY6

; Interrupt control register and control bits
; very important that it is level triggered
INTK_CONTROL EQU INT2_CONTROL
INTK_CONTROL_BITS EQU LEVEL_TRIGGER + UNMASK_INTERRUPT + INTK_PRIORITY

; SerialIO chip assigned PCS2 chip select in IO space
; on our target boards.
; PACS/MACS in ML.inc will show that IO space base is 0000H
PCS2_BASE			 EQU 0100H

; Control and Status register addresses (A2,1,0)
; DLAB only required for Divisor latch setting, assume after initial baud rate
; setup, DLAB set and kept 0 so the address that follow are valid.

; Receiver buffer (when reading)
ADDRESS_RBR EQU PCS2_BASE + 0000H
; Transmitter holding register (when writing)
ADDRESS_THR EQU PCS2_BASE + 0000H
; Interrupt enable
ADDRESS_IER EQU PCS2_BASE + 0001H 
; Interrupt identification (read only)
ADDRESS_IIR EQU PCS2_BASE + 0002H 
; Line control
ADDRESS_LCR EQU PCS2_BASE + 0003H 
; Modem control
ADDRESS_MCR EQU PCS2_BASE + 0004H 
; Line status
ADDRESS_LSR EQU PCS2_BASE + 0005H 
; Modem status
ADDRESS_MSR EQU PCS2_BASE + 0006H 
; Scratch
ADDRESS_SR EQU PCS2_BASE + 0007H 

; Divisor Latch (LSB) with DLAB 1
ADDRESS_DLRL EQU PCS2_BASE + 0000H 
; Divisor Latch (MSB) with DLAB 1
ADDRESS_DLRM EQU PCS2_BASE + 0001H


; IER Settings

; Received data available, Transmitter holding register empty, and 
; receiver line status interrupts enabled, modem status disabled.

IER_SETUP EQU 0000111B

; LCR setup
; Bits 1,0 = 11 implies 8 bits/character
; Bit    2 = 0 	implies 1 stop bit
; Bit    3 = 1  implies a parity bit
; Bit    4 = 0  implies an odd parity check
; Bit    5 = 0 	implies not stick parity, whatever that means
; Bit 	 6 = 0  as we do not want to force a break condition
; Bit    7 = 1 if we're about to access the DLRL/M buffers
; Bit 	 7 = 0 if we aren't.
LCR_DURING_INITIALIZATION 	EQU 10001011B
LCR_WHILE_RUNNING 			EQU 00001011B


; Programming the baud generator
; We have a 9.216 MHz clock in from the CLKOUT line of the 80188.
; We want an 2400bits/s = 1 char/4ms transmission/receiver rate.
; Therefore our desired baud rate for bit transmission is 2.4kHz,
; which yields a 9216000/(16*2400) = 240 = 0F0H divisor. So 
; here we only need to clear the DLRM and set the DLRL to 240,
; which fits in a byte easily.

DLRL_SETTING EQU 0F0H
DLRM_SETTING EQU 000H
 
; LSR meanings and masks
; Bit    0 data ready 1 implies a character has been read (clear RBR read)
; Bit    1 overrun 1 implies we missed a character or more (clear LSR read)
; Bit    2 parity  1 implies a parity error (clear LSR read)
; Bit    3 framing 1 implies invalid stop bit (clear LSR read)
; Bit    4 break   irrelevant as we disabled this interrupt
; Bit    5 THRE  if 1 cleared if THR loaded
; Bit 	 6 if 1 both THR and TSR are empty
; Bit    7 always clear

LSR_DATAREADY EQU 00000001B
LSR_OVERRUNER EQU 00000010B
LSR_PARITYERR EQU 00000100B
LSR_FRAMINGER EQU 00001000B
LSR_BREAKERRO EQU 00010000B
LSR_COMPERROR EQU 00011110B 
LSR_TXHREMPTY EQU 00100000B
; assume for now that when you see the THRE interrupt, writing a new value
; to the THR does not immediately wipe the transmitter shift register,
; as the spec says it loads only when the TSR is idle.

; Interrupt identificiation buffer meanings

; no interrupts pending
IO_NO_Interrupt EQU 00000001B 

; Priority 1 Receiver line status interrupt (implies errors)
; Reset by reading Line status register

IO_ERROR_Interrupt EQU 00000110B

; Priority 2 Receiver data available interrupt
; Reset by reading RBR

IO_Received_Interrupt EQU 00000100B

; Priority 3 Transmitter holding register empty interrupt
; Reset by reading IIR if this was the source, or whenever you write the THR

IO_Transmitted_Interrupt EQU 00000010B

; Priority 4 will not occur with our IER_SETUP



