NAME Motors

CGROUP GROUP CODE
DGROUP GROUP DATA

; Constant dependencies for PWM_Control Timer 1 event handler
; Bit definitions and masks for Parallel IO to motors
; Motor orientation definitions (angles) and 
; cosine table sign mapping.
$INCLUDE (GenConst.inc)
$INCLUDE (Events.inc)
$INCLUDE (Errors.inc)
$INCLUDE (ML.inc)
$INCLUDE (GenInts.inc)
$INCLUDE (GenTimer.inc)    
$INCLUDE (HW4Timer.inc)
$INCLUDE (PIOConst.inc)
$INCLUDE (Motors.inc)

CODE SEGMENT PUBLIC 'CODE'

    ASSUME  CS:CGROUP, DS:DGROUP

; import a cosine table with 1 degree resolution from [0,360] 
; indexed intelligently by degree.
EXTRN Cos_Table:NEAR

; Functional Specification: SetMotorSpeed
;
; Description
;
; The function is passed two arguments. 
; The first argument (speed) is passed in AX and is the absolute speed 
; (0,65534) at which the RoboTrike is to run. This updates the Speed_STATUS
; variable. A speed of 65535 will not change the current speed.
; 
; The second argument (angle) is passed in BX and is the signed angle 
; at which the RoboTrike is to move in degrees, with 0 degrees being
; defined by the diagram in Operation and angles increasing CCW. 
; I realize this isn't as stated in the spec - for justification see
; Operation.
; 
; Angles must be in [-32768, 32767]. 
; An angle of -32768 will not change the current direction of travel.
; Other angles will update the Angle_STATUS variable. 
; 
; There is no error handling.
;
; The three Trike motor PWM duty-cycles and directions
; will be set by the call to this
; function. Within one PWM cycle of this function returning, 
; the motors will be seeing their new driving waveforms. 
;
; This function, and SetLaser, are the only functions which
; modify the shared variables (other than PWMInterruptCount),
; and these are not called upon interrupt, only by the main loop.
; Thus the code is not critical. 
;
; As long as the PWM Timer is the only timer in operation on the motor
; control board, and has top priority, and can nest any lower
; priority interrupts, and so long as the PWM cycle period is larger
; than the time it takes to run the PWMTimer interrupt handler (PWMControl),
; we can guarantee the PWM duty cycle defined by this function is the 
; waveform seen by the motor inputs within a duty-cycle resolution as 
; set by RESOLUTION. 

; ###########
;
; Operation
; A quick word on motor/angle definitions and PWM duty cycle calculations.
; If you look at the test cases, you see that (F = Forward, R = Reverse)
; SetMotorSpeed(65534,0)   sets Motor 1F, 2R, 3R
; SetMotorSpeed(32767,120) sets Motor 1R, 2R, 3F
; From the first, we define our picture of the trike as follows:
;
;            R Motor 1 F 
;
;
;
;
; 			  0 degrees
;
;                |y = 90 d
;       180 d    |         1 d
; 		-------------------- x = 0 degrees
;                |       359 d
;           270 d|                     angles increase counterclockwise
;
;
;
;
;
;     F Motor 3 R         F Motor 2 R
;
;
;
; From the second, which asks the trike to move at 120 degrees,
; we see that 120 degrees corresponds to the picture above at 2pi/3. 
; not, as we were told in the spec for this homework, to a clockwise
; angle definition.
;
; Therefore my angles will be calculated and updated according to
; the picture above. Specifically,

; I'm doing my calculations in polar coordinates, effectively.
; To calculate the orientation angles of the motors,
; we simply recognize that the orientations must be rotationally symmetric.
; That is, if we rotate the trike by 2pi/3 we should not be able to tell
; that we rotated at all (if we just look at the wheels)
; So our motors must be oriented according to third roots of unity.
; There are only three of these, and we can match them visually to motors.

; Motor 1 moves parallel to the X axis (a third root of unity)
;THETA_MOTOR1 EQU 0   	
; Motor 2 forward direction is oriented at 4pi/3 (a third root of unity)
;THETA_MOTOR2 EQU 240
; Motor 3 forward direction is oriented at 2pi/3 (a third root of unity)
;THETA_MOTOR3 EQU 120

; If we assume vector-averaged motor output is proportional to 
; the steady state center-of-mass velocity of the trike,
; then we can calculate the required motor  duty cycle as
; v_k = MotorK_PWMDuty = 100 * dot(v/65534,F_k), 
; where F_k is the unit orientation vector for Motor K
; and v/65534 is unit normalized. 
;
; this works out to 
; v_k = (v / 65534) * |F_k| * |cos(THETA_MOTORK - angle)|
; MotorK_Direction = not sign(cos(THETA_MOTORK - angle)) = 0, + Forward     or 
; 											      		   1, - Backward
; the correctness of the direction calculation above can be verified by 
; the command SetMotorSpeed(-, 120) which yields, as before, Motor 1R, 2R, 3F.
;
; cos(0-120) = cos(-120) = cos(2pi/3) < 0   Motor 1R
; cos(240-120) = cos(120) < 0  				Motor 2R
; cos(120-120) = cos(0) > 0 				Motor 3F
;
; So to calculate the duty cycle and direction of our motors, we need
; simply calculate:

; v' = v / 65534 						in [0,1]
; theta' = THETA_MOTORK - angle mod 360 in [0,359], natural
; direction = not sign(cos(theta')) 	in 0,1, natural
; ct = |cos(theta')| / 32767			in [0,1]
; 
; Note that all modulus operations on angles can easily be calculated.
; If the angle is positive, we calculate DIV angle, 360, 
; which for angles in [0,32767] is guaranteed not to overflow.
; If the angle is negative, we just loop adding 360 to it each time,
; until it's first positive, in which case it's guaranteed in 0,359. 
; 
; Then the fractional duty cycle for motor k, fPWMdc_k, equals
; fPWMdc_k = v' * ct
; 
; this is the multiplication of two positive numbers in [0,1]
; so we can represent v' and ct in unsigned 1.15 notation, U1.15
; that is, each of v' and ct is in a word, looking like
; 				v', ct = (2^0).(2^-1)(2^-2)...(2^-15)
; and we know, when we multiply the two, that fPWMdc_k in U1.15 will yield
; a number in [0,1], which looks like
;  	      fPWMdc_k  = (2^1)(2^0).(2^-1)(2^-2)...(2^-30) in DX:AX
; with the MSB 0 guaranteed.
; Thus we can shift DX left logically by 1, and shift AX right logically
; by 15, so the MSB of AX is in its LSB, then add DX and AX to yield
; 		 	fPWMdc_k  = (2^0).(2^-1)(2^-2)...(2^-15) in U1.15 again
; with error on the order of (2^-15).
;
; Then, we can pick our PWM duty cycle RESOLUTION.
; If, say, we pick a PWM period of RESOLUTION PWM interrupts, 
; frequency, so a full PWM duty cycle with RESOLUTION = 128 interrupts
; and takes 128*Interrupt_Period ms. Say we choose a 5kHz interrupt frequency
; so interrupts occur ever 200us, for 128*200 = 22.6 ms PWM period.
; By choosing RESOLUTION 128 and keeping only integer results from the
; multiplications above, we effectively have a true resolution of 1/128 < 1 
; percent for our 1000/22.6 Hz duty cycle.
; 
; These numbers can change, within reason - it turns out the PWM_Control
; code below takes ~100us to run, worst case, so you can't go below that
; or interrupts start stacking and you never really finish up non-interrupt
; code.

; This requires also
; that timer1 interrupts are highest priority and can interrupt
; any other interrupts that occur, otherwise our actual PWM duty waveform
; will have a beat frequency as we'll miss PWM interrupts (most of my
; code takes ~100us to run). This is discussed further in PWMTimer.inc.

; Say we want ~1/100 resolution but want to space our duty cycle over 
; 2^n interrupts, so it's easy to wrap the count by AND count 2^n-1
; Then we can pick RESOLUTION > 100
; and calculate 
; MotorK_PWMDuty = fPWMdc_k * RESOLUTION = [0,1]*x, x in [0,2^n] 
; and 
; MotorK_Direction = not sign (cos(theta')) in {0,1}

; The calculation of MotorK_PWMDuty takes a little more work, actually.
; fPWMdc_k is in [0,1] in U1.15 notation.
; RESOLUTION is in [0,2^(n+1)-1] where n is MSB # in RESOLUTION_BITS
; So fPWMdc_k * RESOLUTION can be made to look like, via shifting and word MUL,
;   (2^0).(2^-1)(2^-2)...................(2^-15) 	      word in [0,1]
; * (2^n)(2^n-1)(2^n-2)...(2^0).(2^-1)...(2^-(16-(n+1)))  word in [0,2^(n+1)-1]
; ------------------------------------------------------
;   (2^n+1)(2^n)......(2^0).(2^-1).......(2^-30+n) 		0 <= DX:AX <= 2^(n+1)-1
; with MSB guaranteed 0.
; To meet the resolution, we need only save the bits corresponding to
; [n,n-1,...0]. Thus, if RESOLUTION = 128, we'd save a number in [0,128],
; in the naturals, for an actual PWM duty cycle resolution less than 1.
; If RESOLUTION were 1000, we'd save a number in [0,1000] for an actual
; resolution of less than 0.1. 
;
; However, technically, from the point of view of this code,
; RESOLUTION in the range [0,2^15] is acceptable, since it's the
; timer interrupt frequency that is bounded below by the time it takes
; to run PWM_Control (~100us).
; 
; To get our natural MotorK_PWMDuty cycle, then, we simply precalculate
; a shifted RESOLUTION with its MSB at the MSB of a word register.
; For RESOLUTION 128, or 2^7, we'd really have the number 2^15 (RESOLUTION 
; shifted left logically by 8 bits).

; Then we calculate MUL RESOLUTION_SHIFTED, fPWMdc_k
; which is guaranteed not to overflow, and taking the result DX:AX,
; we shift logically DX left by one, and then shift logically AX right by 15
; to get its MSB in its LSB, the add DX, AX to generate
; MotorK_PWMDuty' = (2^n)...(2^0).(2^-1)..(2^(16-(n+1)))
; then we shift MotorK_PWMDuty' right logically by 16-(n+1) to get 
; MotorK_PWMDuty = (2^15)...(2^n+1)(2^n)...(2^0) with bits >= n+1 set to 0.

; Then we just need to make sure if MotorK_PWMDuty = RESOLUTION, a full duty 
; cycle, we never turn the motor off, after some initial not-yet-reset 
; period of no more than 1 PWM duty cycle,
; and if MotorK_PWMDuty = 0, we never turn it on, again after the not-yet-reset
; period.

; So the entirety of our motor direction/calculation code comes down to
; two U1.15 conversions, one U1.15 multiplication and truncation, 
; a few angle modulus calculations, and one cosine lookup and sign calculation
; for each motor, and we have three motors - so we loop!

; Then we 'reset' the motors to their off state, and wait! The PWMControl
; code on timer interrupt will take care of the rest, turning on the motors
; if need be after at most 1 PWM cycle.

; The entirety of our PWM control code, called on PWM timer interrupt,
; boils down to.. have you reached the duty cycle? If so, turn yourself off.
; Otherwise stay on, or if you've reached the 0 count (new cycle) turn on,
; as long as the PWM_Duty is not 0 (always off). Simple!

; We can't have critical code because we'll restrict the PWM Control code
; to not mutate the shared variable OutputBits, which is only mutated
; by SetLaser and SetMotorSpeed. These are only called after
; serial IO delivers a new command, and so are only called from a
; main loop - not on interrupt. Thus they will be executed sequentially,
; and our shared variable OutputBits will never be critical from the point
; of view of any function call. The worst thing that can happen is
; we change OutputBits but the PWM timer interrupt event handler PWMControl
; doesn't actually make the change till the next full PWM Cycle, i.e. 2 ms 
; later. The end user will never notice.
;
; ###########

; Arguments:        AX, unsigned speed in [0,65535]
; 					BX, signed angle in [-32768, 32767]
;
; Return values:    none
; Constants:        SHIFTED_RESOLUTION from Motors.inc
; 					RESOLUTION_BITS
; 					NUM_MOTORS
; 					ANGLE_MOD
; 					SAME_ANGLE
; 					SAME_SPEED
; 				 	MAX_SPEED, MAX_COS
; 					CHIP_8255_OUTPUTB_ADDRESS
; Local Variables:  
; 									AX has normalized speed
;									BX has angle mod 360
; 									CX holds intermediate calculations
; 									DH has building OutputBits
; 									SI has motor byte index
; 									DI has motor word index
; Shared Variables: Laser_STATUS (DS, byte)
; 					Speed_STATUS (DS, word)
; 					Angle_STATUS (DS, word)
; 					MotorK_PWMDuty (DS, word array)
; 					MotorK_ReverseDirectionBits (CS, byte array)
; 					MotorK_ForceOrientation (CS, word array, angle>0 guarantee)
; 					Cos_Table (CS, word array, index by degree with 1deg res)
; Global Variables: none
; Registers used:   AX, BX, CX, DX, SI, DI
; Stack depth:      4 words
; Error handling:   none
; Algorithms:       Holonomic 3-wheel drive implemented as discussed in
; 					operation on the premise that average wheel torque 
; 					is proportional to the net force on the Trike,
; 					and that this net force is proportional to the
; 					velocity in steady-state. 
; Data structures:  1D arrays
; Known bugs:       none
; Limitations:      RESOLUTION is limited by both the system clock (18 MHz) and
; 					my decision to restict myself to word MUL and DIV ops.
; 					Average runtimes for my PWM interrupt handler
; 					code are expected to be in the 10us range,
; 					which means with 200us PWM interrupts, we can afford
; 					approximately 50Hz PWM cycling with 1/100 resolution.
; 					Any more and we start not being able to do much of 
; 					anything before we see another PWM interrupt, much less
; 					and our motor won't be seeing a smooth stepped-down DC in.
; Revision History:
;   12/23/2014 SSundaresh  created

; Pseudocode for SetMotorSpeed (AX = speed, BX = angle)
; I actually did write this first - I realize it doesn't look much like
; pseudocode.
;
; compare AX, SAME_SPEED 			 should we update the Speed_Status
; if equal
; 	AX <- word Speed_STATUS
; else 
;   Speed_STATUS <- AX
;
; compare BX, SAME_ANGLE  			 should we update the Angle_Status
; if equal 
;   BX <- word Angle_STATUS
; else
;   push AX
;   call angleMod360 				 compute angle mod 360 in [0,359] in BX
; 	pop AX
;   Angle_STATUS <- BX
;  									 now AX, BX have valid speed, angle in
; 									 [0,65534] and [0,359]							
;
; push BX 							save the angle mod 360
; XOR DX, DX  						clear DX
; MOV CX, MAX_SPEED 				to normalize (DX:AX = AX)
; call U1_15_Division  				now AX / MAX_SPEED in U1.15 notation
; pop BX  							and we restore the angle mod 360
; 
; 
; MOV DH, Laser_STATUS 				set DH to LaserStatus to effectively
; 								 	clear all but possibly 7th bit of
;									our temporary OutputBits 
;
; SI = 0 	 						counter for motors
; while SI < NUM_MOTORS 			
; 	MOV DI, SI						want angle - THETA_MOTOR_K > 0
; 	ADD DI, SI 						MotorK_ForceOrientation is a word array
; 									but SI is byte index
; 	MOV CL, MotorK_ForceOrientation[DI]
;	INC DI
;   MOV CH, MotorK_ForceOrientation[DI]
; 		 							get THETA_MOTOR_K in CX, 
; 	
;
;   compare CX, BX 	
;   if less than
; 	  push BX
;     SUB BX, CX 
; 	  mov cx, bx
; 	  pop BX  						cx <-|angle-THETA_MOTOR_K|, angle preserved
;   else
;  	  SUB CX, BX 					
; 									now have |angle-THETA_MOTOR_K| in CX
;    								guaranteed in [0,359]
; 	push SI
; 	MOV SI, offset(Cos_Table)
;   ADD SI, CX
; 	ADD SI, CX  					word index = 2*degree
;	MOV CX, word ptr CS:[SI]
;   pop SI							lookup cos(|angle-THETA_MOTOR_K|) 			
; 									preserving SI motor index
;  	
; 	compare CX, 0 					check sign of cosine
; 	if less than,
; 		push BX
; 		MOV BX, offset(MotorK_ReverseDirectionBits)
; 		ADD BX, SI 					byte index SI
; 		MOV BL, CS:[BX]
; 		ADD DH, BL
;		pop BX 						assign motor_SI direction bit, save BX.
; 		
; 		NEG CX 						take |cos|
; 	else
; 		do nothing (forward direction bit stays 0)
;  
;	push DX
; 	push BX
;   push SI
; 	push AX
;
; 	XOR DX, DX
; 	MOV AX, CX
; 	MOV CX, MAX_COS
; 	call U1_15_Division 			now AX has normalized cosine in U1.15
; 	mov cx, ax 						move it back to CX
;
;									now, to recap,
; 									AX has normalized speed
;									BX has angle mod 360
; 									CX has normalized cos(theta_motor_k-angle)
; 									DH has building OutputBits
; 									SI has motor index
; 	pop AX
; 	push AX
; 									now to compute AX * CX = fPWMdc_k
;   
; 	MUL CX 							* AX implied, return value DX:AX
; 	SHL DX, 1 						MSB DX = 0 necessarily
; 	SHR AX, 15 						MSB AX -> LSB
;   ADD DX, AX
; 	MOV CX, DX 						CX contains U1.15 fPWMdc_k now
;	
; 	MOV AX, SHIFTED_RESOLUTION
;  	MUL CX
; 	SHL DX, 1
;   SHR AX, 15
;   ADD DX, AX
; 	SHR DX, FINAL_SHIFT
; 	MOV CX, DX 					 	floor(RESOLUTION * fPWMdc_k) in CX
;
; 	pop AX
;  	pop SI 							restore SI byte index
; 	MOV DI, SI
; 	ADD DI, SI 						DI is now word index
;	MOV MotorK_PWMDuty[DI], CL
; 	IND DI
;	MOV MotorK_PWMDuty[DI], CH 		
;
;  	INC SI
; 	 								SI <- SI + 1 now, so onto next motor

;   pop BX 							restore remaining registers
;   pop DX 							AX has normalized speed still
; 									BX has angle mod 360
; 									DX has building OutputBits
; 									SI has motor byte index
;endwhile
;
;MOV AL, DH
;MOV OutputBits, AL 				write OutputBits to memory
;MOV DX, CHIP_8255_OUTPUTB_ADDRESS
;OUT DX, AL  						write laser/direction bits to 8255
; 									with all motors off (never set)
;return

SetMotorSpeed PROC NEAR
 			  PUBLIC SetMotorSpeed
CMP AX, SAME_SPEED  			;should we update the Speed_Status
JE KEEP_SPEED
JNE UPDATE_SPEED
KEEP_SPEED:
	PUSH BX
	MOV BX, offset(Speed_STATUS)
	MOV AL, DS:[BX]
	INC BX 						; low byte then high byte read
	MOV AH, DS:[BX]
	POP BX
	JMP DEAL_WITH_ANGLE
UPDATE_SPEED:
	PUSH BX
	MOV BX, offset(Speed_STATUS)
	MOV DS:[BX], AL
	INC BX 						; low byte then high byte written
	MOV DS:[BX], AH
	POP BX
	JMP DEAL_WITH_ANGLE

DEAL_WITH_ANGLE:

CMP BX, SAME_ANGLE  			;should we update the Angle_Status
JE KEEP_ANGLE
JNE UPDATE_ANGLE
KEEP_ANGLE:
	MOV SI, offset(Angle_STATUS)
	MOV BL, DS:[SI]
	INC SI
	MOV BH, DS:[SI]
	JMP NORMALIZE_SPEED
UPDATE_ANGLE:
	PUSH AX
   	CALL angleMod360 				 ;compute angle mod 360 in [0,359] in BX
 	POP AX
 	MOV SI, offset(Angle_STATUS)
 	MOV DS:[SI], BL
 	INC SI
 	MOV DS:[SI], BH
  									; now AX, BX have valid speed, angle in
 									; [0,65534] and [0,359]							

	JMP NORMALIZE_SPEED

NORMALIZE_SPEED:

PUSH BX 							;save the angle mod 360
XOR DX, DX  						;clear DX
MOV CX, MAX_SPEED 					;to normalize (DX:AX = AX)
CALL U1_15_Division  				;now AX / MAX_SPEED in U1.15 notation
POP BX  							;and we restore the angle mod 360

 
MOV DH, Laser_STATUS 				;set DH to LaserStatus to effectively
 								 	;clear all but possibly 7th bit of
									;our temporary OutputBits 

MOV SI, 0	 						;counter for motors
MOTOR_LOOP:			
 									;want angle - THETA_MOTOR_K > 0
 	MOV DI, offset(MotorK_ForceOrientation)
    ADD DI, SI
    MOV CL, CS:[DI]
 	XOR CH, CH
		 							;get THETA_MOTOR_K in CX 	

   CMP CX, BX 	
   JL ANGLE_CASE1
   JGE ANGLE_CASE2

   ANGLE_CASE1:
 		PUSH BX
    	SUB BX, CX 
 		MOV CX, BX
 		POP BX  					;cx <-|angle-THETA_MOTOR_K|, angle saved
 		JMP GET_COSINE
   ANGLE_CASE2:
		SUB CX, BX 					
		JMP GET_COSINE
 									;now have |angle-THETA_MOTOR_K| in CX
    								;guaranteed in [0,359]
    GET_COSINE:
 	PUSH SI
 	MOV SI, offset(Cos_Table)
   	ADD SI, CX
 	ADD SI, CX  					;word index = 2*degree
	MOV CX, word ptr CS:[SI]
  	POP SI							;lookup cos(|angle-THETA_MOTOR_K|) 			
 									;preserving SI motor index
  	
 	CMP CX, 0 	 					;check sign of cosine
 	JL NEGATIVE_COSINE
 	JGE NORMALIZE_COSINE

 	NEGATIVE_COSINE:
 		PUSH BX
 		MOV BX, offset(MotorK_ReverseDirectionBits)
 		ADD BX, SI 					;byte index SI
 		MOV BL, CS:[BX]
 		ADD DH, BL
		POP BX 						;assign motor_SI direction bit, save BX.
 		
 		NEG CX 						;take |cos|
 		JMP NORMALIZE_COSINE
 	
 	NORMALIZE_COSINE:
 	
	PUSH DX
 	PUSH BX
	PUSH SI
 	PUSH AX

 	XOR DX, DX 					 	; setup U1_15_Division arguments
 	MOV AX, CX
 	MOV CX, MAX_COS
 	CALL U1_15_Division 			;now AX has normalized cosine in U1.15
 	MOV CX, AX 						;move it back to CX

									;now, to recap,
 									;AX has normalized speed
									;BX has angle mod 360
 									;CX has normalized cos(theta_motor_k-angle)
 									;DH has building OutputBits
 									;SI has motor index
 	POP AX
 	PUSH AX
 									;now to compute AX * CX = fPWMdc_k
   
 	MUL CX 							;* AX implied, return value DX:AX
 	SHL DX, 1 						;MSB DX = 0 necessarily
 	SHR AX, 15 						;MSB AX -> LSB
	ADD DX, AX
 	MOV CX, DX 						;CX contains U1.15 fPWMdc_k now
	
 	MOV AX, SHIFTED_RESOLUTION
  	MUL CX
 	SHL DX, 1
	SHR AX, 15
	ADD DX, AX
 	SHR DX, FINAL_SHIFT
 	MOV CX, DX 					 	;floor(RESOLUTION * fPWMdc_k) in CX

 	POP AX
  	POP SI 							;restore SI byte index
 	
    MOV DI, offset(MotorK_PWMDuty)
    ADD DI, SI
 	ADD DI, SI 						;DI is now word index
    
	MOV DS:[DI], CL
 	INC DI                          ; write updated duty cycle for this motor
	MOV DS:[DI], CH
    
  	INC SI
 	 								;SI <- SI + 1 now, so onto next motor

   POP BX 							;restore remaining registers
   POP DX 							;AX has normalized speed still
 									;BX has angle mod 360
 									;DX has building OutputBits
 									;SI has motor byte index
	CMP SI, NUM_MOTORS
	JE UPDATE_MOTOR_CONTROL_BITS
	JMP MOTOR_LOOP

UPDATE_MOTOR_CONTROL_BITS:
MOV AL, DH
MOV OutputBits, AL 					;write OutputBits to memory
MOV DX, CHIP_8255_OUTPUTB_ADDRESS
OUT DX, AL  						;write laser/direction bits to 8255
 									;with all motors off (never set)
RET		 
SetMotorSpeed ENDP


; MotorK_ForceOrientation 
;
; Description:      Trike motor effective force vector polar angle
; 					indexed by motor (0,1,2 -> Motors 1,2,3)
; 					Byte array, positive angles within 0,255 degrees only
; Revision History: 12/23/2014 SSundaresh created

MotorK_ForceOrientation			LABEL   BYTE
								PUBLIC  MotorK_ForceOrientation
	DB THETA_MOTOR1
	DB THETA_MOTOR2
	DB THETA_MOTOR3

; MotorK_ReverseDirectionBits 
;
; Description:      Trike motor direction control bits for reverse drive.
; 					indexed by motor (0,1,2 -> Motors 1,2,3)
; 					Byte array
; Revision History: 12/23/2014 SSundaresh created

MotorK_ReverseDirectionBits		LABEL   BYTE
								PUBLIC  MotorK_ReverseDirectionBits
	DB PORTB_M1DIR
	DB PORTB_M2DIR 
	DB PORTB_M3DIR 

; MotorK_DriveBits
;
; Description:      Trike motor drive control bits.
; 					indexed by motor (0,1,2 -> Motors 1,2,3)
; 					Byte array
; Revision History: 12/23/2014 SSundaresh created

MotorK_DriveBits		LABEL   BYTE
								PUBLIC  MotorK_DriveBits
	DB PORTB_M1OOF
	DB PORTB_M2OOF 
	DB PORTB_M3OOF 


; Functional Spec: MotorsInit		
; Description
;	laser status cleared, 
; 	speed status set to 0, 
;	angle status set to 0
;	OutputBits cleared
;	motork pwm duty array at 0
;	pwm interrupt count at 0
; Registers used: BX for addressing, AX as index
; Constants: NUM_MOTORS
; Stack Depth: 1 word
; Shared Variables: 
; 	Laser_STATUS
;	Speed_STATUS
;	Angle_STATUS
; 	OutputBits
; 	MotorK_PWMDuty
;	PWMInterruptCount
; Last Revised: 12/23/2014 SSundaresh created
MotorsInit 	PROC NEAR
			PUBLIC MotorsInit
MOV AL, 0

MOV BX, offset(Laser_STATUS) 			; clear laser status byte
MOV DS:[BX], AL

MOV BX, offset(Speed_STATUS)
MOV DS:[BX], AL
INC BX 									; clear Speed_STATUS word
MOV DS:[BX], AL

MOV BX, offset(Angle_STATUS)
MOV DS:[BX], AL
INC BX 									; clear Angle_STATUS word
MOV DS:[BX], AL

MOV BX, offset(OutputBits)
MOV DS:[BX], AL 						; clear OutputBits byte

MOV BX, offset(PWMInterruptCount)
MOV DS:[BX], AL
INC BX 									; clear PWMInterruptCount
MOV DS:[BX], AL

MOV BX, offset(MotorK_PWMDuty)

MOTOR_PWM_INIT_LOOP:
	PUSH AX
	MOV AL, 0
	MOV DS:[BX], AL
	INC BX
	MOV DS:[BX], AL
	INC BX
	POP AX

	INC AL
	CMP AL, NUM_MOTORS
	JL MOTOR_PWM_INIT_LOOP
	JE Motors_Init_Done

Motors_Init_Done:
RET
MotorsInit ENDP

; Functional Spec: GetMotorSpeed
; Description: Returns Speed_STATUS in AX
; 				This is guaranteed to be in [0,65534].
; Registers Used: BX, AX (BX restored before returning)
; Stack Depth: 1 word
; Known Bugs: none
; Last Revised: 12/23/2014 SSundaresh created
GetMotorSpeed	PROC NEAR
				PUBLIC GetMotorSpeed
PUSH BX
MOV BX, offset(Speed_STATUS)
MOV AL, DS:[BX] 					; Speed_STATUS is stored low/high byte
INC BX
MOV AH, DS:[BX]
POP BX
RET 
GetMotorSpeed ENDP
	
; Functional Spec: GetMotorDirection
; Description: Returns Angle_STATUS in AX
; This is guaranteed to be in [0,359].
; However, angles are not measured as per the given spec.
; What I mean by 0, and why I increment angles CCW, instead of clockwise,
; is described in SetMotorSpeed Operation. I believe
; my approach is consistent with the test cases that were described to us.
; 
; Registers Used: BX, AX (BX restored before returning)
; Stack Depth: 1 word
; Known Bugs: none
; Last Revised: 12/23/2014 SSundaresh created
GetMotorDirection	PROC NEAR
				PUBLIC GetMotorDirection
PUSH BX
MOV BX, offset(Angle_STATUS)		; Angle_STATUS is stored low/high byte
MOV AL, DS:[BX]
INC BX
MOV AH, DS:[BX]
POP BX
RET 
GetMotorDirection ENDP

; Functional Spec: SetLaser
; Description:
; 	Updates MSB of control byte OutputBits to set the laser on or off,
; 	depending on its input in AX. If AX > 0, laser on. Otherwise laser off.
; 	No return value. Updates Laser_STATUS.
; Registers Used: AX, mutated from input
; Stack Depth: 0 words
; Known bugs: none
; Last Revised: 12/23/2014 SSundaresh created
SetLaser PROC NEAR
		PUBLIC SetLaser
CMP AX, 0
JE TURN_LASER_OFF
JNE TURN_LASER_ON

TURN_LASER_OFF:	
	MOV Laser_STATUS, 0
	MOV AL, OutputBits
	AND AL, LASER_MASK	 		; clear the laser bit, preserving the rest
	MOV OutputBits, AL 			
	JMP DONE_SET_LASER	 	

TURN_LASER_ON:
	MOV Laser_STATUS, PORTB_LASER 	; greater than 0, see GetLaser and DS
	MOV AL, OutputBits
	AND AL, LASER_MASK			; clear the laser bit, preserving the rest
	ADD AL, PORTB_LASER 		; write the laser bit, preserving the rest
	MOV OutputBits, AL 			; write updated control byte to memory
	JMP DONE_SET_LASER	

DONE_SET_LASER: 				; laser will turn on at next PWM interrupt
RET
SetLaser ENDP

; Functional Spec: GetLaser
; Description: Returns Laser_STATUS in AX
; This is guaranteed to be 0 if the laser is off, and greater than 0
; if the laser is on.
; 
; Registers Used: BX, AX (BX restored before returning)
; Stack Depth: 1 word
; Known Bugs: none
; Last Revised: 12/23/2014 SSundaresh created
GetLaser	PROC NEAR
				PUBLIC GetLaser
PUSH BX
MOV BX, offset(Laser_STATUS)
MOV AL, DS:[BX] 		; AL contains Laser Status bit which is > 0 iff 
						; the laser is on.
XOR AH, AH 				; clear AH
POP BX
RET 
GetLaser ENDP

; Functional Spec angleMod360
; Description:
; 	Given a signed word angle in [-32767,32767] in BX,
; 	returns the angle modulo 360 in [0,359], also in BX.
; Operation:  		If BX < 0, add ANGLE_MOD until it is first GEQ 0
; 					If BX >= ANGLE_MOD, subtract ANGLE_MOD until it
; 					is first LEQ ANGLE_MOD. Return this value in BX.
; Arguments:        BX: signed word angle in [-32767,32767]
; Return values:    BX: positive word angle in [0,359]
; Constants:        ANGLE_MOD
; Local Variables:  BX: angle 
; Shared Variables: none
; Global Variables: none
; Registers used:   BX
; Stack depth:      0 words
; Error handling:   none
; Algorithms:       none
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   12/23/2014 SSundaresh  created
angleMod360 PROC NEAR
			PUBLIC angleMod360
ANGLE_MOD_LOOP:
CMP BX, 0
JL TURN_CCW
JGE SHOULD_WE_TURN_CW

TURN_CCW:
ADD BX, ANGLE_MOD
JMP ANGLE_MOD_LOOP

SHOULD_WE_TURN_CW:
CMP BX, ANGLE_MOD
JL JOBS_DONE
JGE YES_TURN_CW

YES_TURN_CW:
SUB BX, ANGLE_MOD
JMP ANGLE_MOD_LOOP

JOBS_DONE:
RET
angleMod360 ENDP


; Functional Spec U1_15_Division
; Description:
; This function takes inputs CX, AX, DX with DX = 0, and CX, AX in [0,65534]
; with CX >= AX. It computes DX:AX / CX and returns a result in U1.15
; notation, that is, an unsigned number of the form (2^0).(2^-1)...(2^-15)
; with error on the order of O(2^-15).
; There is no error checking but valid input cannot overflow.
; The function uses the register BX and does not restore it itself.
; The return value is in AX.
; 	
; Operation:  		
; 	Clear the output holder BX.
; 	Track which base2-bit we're on. Initially, 2^0's place, or cnt = 0.
;
; 	DX starts out 0, and AX < CX initially.
; 	Divide DX:AX by CX.
; 	The quotient is guaranteed {0,1}; a single bit in AX. The remainder in DX
; 	is guaranteed less than CX; moreover, suppose we multiplied DX by 2, 
; 	and found a quotient larger than 1*CX - that is, 2*DX >= 2*CX. Then 
; 	DX >= CX, which is a contradiction. So if we were to divide 2*DX by CX,
; 	we're sure to receive a quotient in {0,1} and a remainder less than CX.
; 	
; 	Now, shift the quotient by the base-2 unit we're on. Currently we're on
; 	base-2 unit 2^0, and since our output must be U1.15, we want this bit
; 	as the MSB.
; 	
;	So we shift the single bit quotient AX by 15 bits to the MSB of its word.
; 	In general if we were on the 2^cnts place, we'd shift the quotient by
; 	15-cnt. 
; 	
; 	Then, we preserve this bit value in our U1.15 representation
; 	by ORing with BX, our placeholder.
;
; 	Then we simply multiply our remainder in DX by two, keeping in mind
; 	that this need not yield a single word result, but could spill over
; 	into two words - only 2*DX < 2*CX still holds. 
; 	We're now on the next unit in base 2, so we update our count,
; 	and we iterate, only on this second iteration DX is not required to be
; 	0, it is whatever the result of MUL DX, 2 has set it to be, as is AX.
; 	CX remains the same, as this is standard long division in base 2.
; 	
; 	When we've computed all 16 places we're going to, we stop, and 
; 	shift the return value from BX to AX, then return.
; Arguments:        AX: numerator, in [0,65534] <= CX
;					DX: 0
;					CX: denominator, in [0,65534]
; 					
; Return values:    AX: AX/CX in U1.15 notation.
; Constants:        none
; Local Variables:  DX:AX always contains current numerator, CX contains same
; 					denominator throughout. BX is a holder for the division
; 					in U1.15 notation. SI is the count, the base2-unit
; Shared Variables: none
; Global Variables: none
; Registers used:   AX, BX, CX, DX, SI
; Stack depth:      1 word
; Error handling:   none
; Algorithms:       long division with truncation at the 2^-16's place.
; Data structures:  none
; Known bugs:       none
; Limitations:      none
; Revision History:
;   12/23/2014 SSundaresh  created
; Pseudocode
;		SI = 0 						 		keep track of base-2 place
;		clear bx 								bx will hold our result for now
;		LoopST: 								
;		if SI < 16 							we only need 15 bit precision
;			DIV DX:AX / CX 
;			AX <- quotient in {0,1} guaranteed
;			DX <- remainder in [0,CX-1]
; 			push cx
; 			mov cx, 15
; 			sub cx, SI 						shift the quotient to the right
;			SHL AX, CX 						location for bx storage
; 			pop cx 							
;			OR BX, AX 						preserve result for this bit
;
;			MOV AX, DX 						now work with the remainder
; 			push CX
; 			MOV CX, 2 						
; 			MUL CX 							word multiplication with Ax
; 			pop CX 							yields product in DX:AX as desired
;
;			INC SI 							on to the next bit!
;			jump to LoopST
;		else 
;		 	mov AX, BX 						we are done, want output in AX
;			return 

U1_15_Division PROC NEAR
			PUBLIC U1_15_Division
MOV SI, 0 						 	;keep track of base-2 place
XOR BX, BX 							;bx will hold our result for now
DIVISION_LoopST: 							

	DIV CX 
	PUSH CX                         ; save CX denominator
    MOV CX, SI                      ; get base2-unit in cx,
    SHL CX, 8                       ; move it to ch
 	MOV CL, 15                      ; move 15 to cl, then compute cl - ch.
	SUB CL, CH 	 					;or how much to shift the quot to the right
	SHL AX, CL 						;location to be stored in bx now possibly
	POP CX 							;active (but only this bit).
	OR BX, AX 						;preserve result for this bit

	MOV AX, DX 						;now work with the remainder
	PUSH CX
	MOV CX, 2 						
	MUL CX 							;word multiplication with Ax
	POP CX 							;yields product in DX:AX as desired

	INC SI 							;on to the next bit!

CMP SI, 16
JL DIVISION_LoopST					;we only need 15 bit precision
JE DIVISION_DONE

DIVISION_DONE:
MOV AX, BX 							;we are done, want output in AX
	
RET
U1_15_Division ENDP

; Functional Spec: PWM_Control
; Description:
; 	Called only on PWM Timer interrupts.
; 	Does not mutate OutputBits. Only reads it.
; 	
; 	Counts PWM interrupts off PWM Timer, wrapping at RESOLUTION.
; 	If the PWM duty cycle for a motor is not 0, 
; 	when the count reaches 0, the motor is turned on.
; 	Until the coutn reaches the PWM duty count, the motor stays on.
; 	When the count reaches the PWM duty count, the motor is turned off.
; 	Thereafter it stays off. 
;
;   If the PWM duty cycle for a motor is 0,
; 	The motor is never turned on after it is turned off by SetMotorSpeed.
;
; 	If the PWM duty cycle for a motor is RESOLUTION, it's duty cycle is 100
; 	and it is never turned off.

; 	After these conditions are checked (and the output bits appropriately 
; 	set in AL), the appropriate control byte is written to the 8255.
; Operation: This function needs to take less than PWM interrupt rate to 
; 	complete or else we'll miss PWM interrupts. 
;
; 	Get the PWMInterruptCount.
; 	Increment this count and wrap at RESOLUTION-1.
; 	Write the updated count to PWMInterruptCount.
;
;	Read the control byte OutputBits into AL. Remember that in all our code,
; 	OutputBits always has the motor drive bits set off. So we cannot write
; 	OutputBits directly to the 8255 here.
;
;	For each motor, read in its duty cycle (DC) in [0,Resolution]
; 	If this DC != 0, we might possibly want to turn the motors on.
; 	If this DC = 0 , we will never want to turn the motors on, so we should
; 	just write the control byte as is. 
;
;  	But, if DC != 0, we check if the count is less than the duty cycle for that
; 	motor, in which case we turn the motor on, by adding the motor drive bit
; 	that is appropriate to the control byte AL.
; 	If the count is greater than or equal to the duty cycle, we want to turn
; 	the motor off, so we simply pass, keeping the motor drive bit 0 (default).
; 	
; 	After looping through the motors in this way, we take our built up 
; 	control byte AL and write it to CHIP_8255_OUTPUTB_ADDRESS on an IO cycle
; 	to program the output of the 8255 parallel IO driving the motors on the 
; 	Trike.
; 	
; 	It then sends the appropriate Timer EOI and returns re-enabling interrupts.
; Arguments:       	None	 					
; Return values:    None
; Constants:        CHIP_8255_OUTPUTB_ADDRESS, NUM_MOTORS
; 					MotorK_DriveBits (CS, byte array indexed by motor)
; 					EOI
; 					EOI_TIMER
; Local Variables:  
; 	AX control byte
; 	BX general purpose addressing
; 	CX motor duty cycles
; 	DX interrupt count holder, 8255 addressing, timer EOI addressing
; 	SI motor index
; Shared Variables: MotorK_PWMDuty, OutputBits, PWMInterruptCount
; Global Variables: none
; Registers used:   DX, AX, BX, CX, SI
; Stack depth:      8 words, flags
; Error handling:   None
; Algorithms:       None
; Data structures:  none
; Known bugs:       none
; Limitations:      must run in less than PWMTimer period, so its complexity
; 					bounds the PWMTimer period from below.
; Revision History:
;   12/23/2014 SSundaresh  created
; Pseudocode
; PWM_Control		
;		inc count 						    
;		wrap at resolution-1                								    
;		write count		
;
;		get control word in register
;
;		for k = 1,2,3 			
;			if mKpwmduty != 0 
;				if cnt < mKpwmduty
;					mask on the motor k on bit.					
;				elseif cnt >= mKpwmduty		
;					mask off the motor K on bit.
;			else
;				do nothing
;
;		write control word to 8255	
;		return
	
		
PWM_Control PROC NEAR
 			PUBLIC PWM_Control
           
PUSHA
MOV BX, offset(PWMInterruptCount)
MOV DL, DS:[BX]
INC BX 									; get PWMInterruptCount
MOV DH, DS:[BX]

INC DX
CMP DX, RESOLUTION_WRAP
JLE WRITE_INTERRUPT_COUNT
MOV DX, 0
                                        ; wrap count at Resolution-1
WRITE_INTERRUPT_COUNT:
MOV BX, offset(PWMInterruptCount)
MOV DS:[BX], DL
INC BX 									; not critical as timers cannot usually
MOV DS:[BX], DH 						; be interrupted (highest priority set)

MOV AL, OutputBits 						; get base control byte

MOV SI, 0 								; motor index
PWM_LOOP_ST:
	MOV BX, offset(MotorK_PWMDuty)
	ADD BX, SI
	ADD BX, SI
	MOV CL, DS:[BX]
	INC BX
	MOV CH, DS:[BX] 					; now CX has the Motor's PWM duty

	CMP CX, 0 							; compare motor PWM duty with 0
	 									; if duty = 0, never want to turn on
	 									; o/w might consider it.
	JE NEXT_MOTOR
	JNE MORE_CHECKS

	MORE_CHECKS:
	CMP DX, CX 							; compare the interrupt count and duty
	JL	TURN_MOTOR_ON
	JGE NEXT_MOTOR 						; control bit off by default.

	TURN_MOTOR_ON:
	MOV BX, offset(MotorK_DriveBits)
	ADD BX, SI
	ADD AL, CS:[BX]
	JMP NEXT_MOTOR
	
	NEXT_MOTOR:
	INC SI
	CMP SI, NUM_MOTORS
	JL PWM_LOOP_ST
	JE WRITE_CONTROL_AS_IS

WRITE_CONTROL_AS_IS:
MOV DX, CHIP_8255_OUTPUTB_ADDRESS
OUT DX, AL 								; write control byte to 8255

MOV     DX, EOI        ;send specific EOI to the int controller
MOV     AX, EOI_TIMER
OUT     DX, AX

POPA

IRET
PWM_Control ENDP


CODE ENDS

;Shared Variables
DATA    SEGMENT PUBLIC  'DATA'

;Bits to write to 8255. See PIOConst.inc for bit 
;definitions
OutputBits DB ?

; Speed, Angle are word quantities in [0,MAX_SPEED] arbitrary units,
; and [0,MAX_ANGLE] degrees, respectively
; These must be written low then high byte for proper operation.
Speed_STATUS DW ?
Angle_STATUS DW ?

; Laser status is either the nonzero byte constant PORTB_LASER
; If you consider AH = 0, AH:PORTB_LASER, you have a number > 0 so
; returning just Laser_STATUS in AX will suffice for GetLaser(), above.
; or identically 0. 
Laser_STATUS DB ?

; PWM Duty Cycle Array of words to match the InterruptCount and Resolution
; The array is ordered Motor 1,2,3, which are described, relative to the
; angle orientation, in Motors.inc. All words are low, then high byte.
MotorK_PWMDuty DW NUM_MOTORS DUP (?)	

; InterruptCounter off the PWM Timer, 1, as a word to ensure Resolution
; for PWM can be extended beyond byte quantities if necessary (
; and if the system clock permits).
PWMInterruptCount DW ?

DATA    ENDS

END	