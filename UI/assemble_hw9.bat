REM NAME assemble_HW9 Motors

REM Description: Batch file to build x86 assembly code for input to pcdebug
REM Revision History: 
REM 10/24/2014 Sushant Sundaresh created for hw2 of EE51, Fa2014, Caltech
REM 11/14/2014 Sushant Sundaresh modified for hw4
REM 11/15/2014 Sushant Sundaresh expanded files for hw4
REM 12/17/2014 SSundaresh 		 expanded for hw5
REM 12/23/2014 SSundaresh 		 updated for hw6
REM 12/25/2014 SSundaresh 		 updated for hw7
REM 12/25/2014 SSundaresh 		 updated for hw8
REM 12/31/2014 SSundaresh 		 updated for hw9 motors
REM 12/31/2014 SSundaresh 		 updated for hw9 ui

REM Operation: save this file in the same U:\ directory as the rest of the code
REM run it as assemble_hw9.bat
REM then input, in pcdebug
REM load hw9u
REM set n ML.breaklabel
REM go, step, trace, etc.

asm86chk ChipSel.asm
asm86chk converts.asm
asm86chk DFiles.asm
asm86chk Errors.asm
asm86chk Handlers.asm
asm86chk IntSetup.asm
asm86chk KPFuncs.asm
asm86chk ML.asm
asm86chk Parser.asm
asm86chk queues.asm
asm86chk segtable.asm
asm86chk SerialCS.asm
asm86chk SerialIO.asm
asm86chk TFiles.asm
asm86chk UIEvents.asm

asm86 ChipSel.asm m1 db ep
asm86 converts.asm m1 db ep
asm86 DFiles.asm m1 db ep
asm86 Errors.asm m1 db ep
asm86 Handlers.asm m1 db ep
asm86 IntSetup.asm m1 db ep
asm86 KPFuncs.asm m1 db ep
asm86 ML.asm m1 db ep
asm86 Parser.asm m1 db ep
asm86 queues.asm m1 db ep
asm86 segtable.asm m1 db ep
asm86 SerialCS.asm m1 db ep
asm86 SerialIO.asm m1 db ep
asm86 TFiles.asm m1 db ep
asm86 UIEvents.asm m1 db ep

link86 ChipSel.obj, converts.obj, DFiles.obj, Errors.obj to hw91.lnk
link86 Handlers.obj, IntSetup.obj, KPFuncs.obj, ML.obj to hw92.lnk
link86 Parser.obj, queues.obj, segtable.obj, SerialCS.obj to hw93.lnk
link86 SerialIO.obj, TFiles.obj, UIEvents.obj, hw91.lnk, hw92.lnk, hw93.lnk to hw9u.lnk

loc86 hw9u.lnk to hw9u NOIC AD(SM(CODE(1000H), DATA(400H),STACK(7000H)))
pcdebug -p COM1




