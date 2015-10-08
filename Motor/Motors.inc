; NAME Motors.inc
; Contains:
; motor angles of orientation,
; PWM resolution
; angle modulus constants
; angle, speed status update constants
;
; Last Revision SSundaresh 12/23/2014

; See Functional Spec for SetMotorSpeed to understand where
; these angles come from, and why I'm using polar coordinates
; instead of cartesian force vectors.
; Motor 1 moves parallel to the X axis (a third root of unity)
THETA_MOTOR1 EQU 0   	
; Motor 2 forward direction is oriented at 4pi/3 (a third root of unity)
THETA_MOTOR2 EQU 240
; Motor 3 forward direction is oriented at 2pi/3 (a third root of unity)
THETA_MOTOR3 EQU 120
; These angles are required positive for the code in SetMotorSpeed to work
; as spec'd.

; RESOLUTION > 100. We will wrap interrupt counts in [0,RESOLUTION_WRAP].
; RESOLUTION > 100 implies we have PWM duty cycle resolution of about 1/100 
; In this case, RESOLUTION = 00A0H = 2^7 + 2^5 = 128 + 32 = 160 
; so our true duty cycle resolution is 0.625 percent.
; from HW4Timer.inc, with our PWM timer interrupts at 200us intervals,
; we see that 160*200us = 32 ms PWM cycles or 1000/32 = 31 Hz 
; PWM duty cycles. Not very high.. but maybe enough.

RESOLUTION EQU 00A0H
RESOLUTION_WRAP EQU RESOLUTION-1
SHIFTED_RESOLUTION EQU 0A000H
; highest power of n in Resolution (2^7, here, so 7)
RESOLUTION_BITS EQU 7
; see SetMotorSpeed operation - this is 16-(n+1)
FINAL_SHIFT EQU 15-RESOLUTION_BITS

; we have three motors (size of motor state arrays)
NUM_MOTORS EQU 3

; calculate ANGLE mod ANGLE_MOD
ANGLE_MOD EQU 360
; if we see SAME_ANGLE as an input we shouldn't change the angle.
SAME_ANGLE EQU 32768
; cos value to normalize by
MAX_COS EQU 32767
; what we should normalize speeds by
MAX_SPEED EQU 65534
; if we see SAME_SPEED we shouldn't change the speed.
SAME_SPEED EQU 65535

; FORWARD is 0 bit for direction in 8255 output
MOTOR_FORWARD EQU 00H
