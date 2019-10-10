;************************************************************************
;*                                                                      *
;* ADDFN - static Cobol interface to external functions.                *
;* Copyright (C) Micro Focus 1988                                       *
;*                                                                      *
;*      Places where the user might wish to alter this file are         *
;*      marked with '!!!'                                               *
;*                                                                      *
;*      This example interfaces to OS/2 XXX calls                       *
;*              XXX can be:  DOS, VIO, KBD, MOU                         *
;*                                                                      *
;*      Use:                                                            *
;*              This file must be linked with                           *
;*                                                                      *
;*          LINK install addfn,cobolxxx,,doscalls,cobolxxx.def;       *
;*                                                                      *
;*              to create COBOLXXX.DLL.                                 *
;*                                                                      *
;*          COBOLXXX can then be called from Animator (using DO ...)    *
;*          or called by the user program.  This will make all the      *
;*          XXX calls listed below available to the Animated program.   *
;*                                                                      *
;*                                                                      *
;*          A MAKE file: INSTALLF is provided which can be used to      *
;*          build COBOLXXX.DLL from INSTALL.OBJ and ADDFN.ASM           *
;*                                                                      *
;*          MAKE CALLSET=XXX COBOLOS2                                   *
;*                                                                      *
;*          If you want to create COBOLALL.DLL, containing all the      *
;*          DOS, VIO, KBD and MOU calls, simply use:                    *
;*                                                                      *
;*          MAKE INSTALLF                                               *
;*                                                                      *
;************************************************************************
.lall
.lfcond

message macro   strings
        IRP x,<strings>
        db  '&x'
        ENDM
        endm

setif   macro   c1,c2,t,s1,s2           ;;  Macro to set t to S1 or S2
        ifdif   <c1>,<c2>               ;;  If c1 and c2 are identical
            t   equ <s1>                ;;  t becomes S1
        else
            t   equ <s2>                ;;  t becomes S2
        endif
        endm

checknames  macro   name, names
            IRP x,<names>
            ifdef x
                name    equ <x>
            endif
            ENDM
            endm

includeall  macro   names               ;;  Macro to include many files
            IRP x,<names>               ;;  names is a list of files to include
            IF1                         ;;  Show which files are being included
                %OUT Including x.inc
            ENDIF
include     x.inc                       ;;  Include the file
            ENDM
            endm

publiclabel macro   firstbit,secondbit  ;;  Make a public label
            public  firstbit&secondbit
firstbit&secondbit:
            endm

FUNCTIONLIST    equ     <DOS,VIO,KBD,MOU>

ifndef  CALLSET
        checknames CALLSET,%FUNCTIONLIST
        ifndef  CALLSET
            CALLSET equ <API>           ;   Default to all OS/2 CALLs
        endif
endif

                                        ;   if ALL, include DOS, VIO, KBD, MOU
setif   %CALLSET,<API>,CALLFNS,%CALLSET,%FUNCTIONLIST

include     addfun.mac                  ;   Include the ADDFUN macro

extrn   $$COBOLINSTALL:far              ;   The install routine

_TEXT   segment word    public 'CODE'   ;   Define _TEXT with class CODE
        assume cs:_TEXT

ifdef NAME
publiclabel %NAME
else
publiclabel COBOL,%CALLSET              ;   Define the .DLL entry point
endif
        jmp $$COBOLINSTALL              ;   And install the functions.

;************************************************************************
;*                                                                      *
;* !!!  Any routines needed before the functions can be added here.     *
;*          (Outward alterations are possible, but more difficult.)     *
;*                                                                      *
;************************************************************************
extrn   $$USERSCREEN:far        ;   Call this if the User screen is required
                                ;   for the action of the call
                                ;   eg  Vio changing the screen
                                ;       Kbd needing the screen
                                ;       Mou needing the screen
                                ;   NB  Animator will automatically restore
                                ;       the screen when it regains control.
                                ;   WARNING:
                                ;       the $$USERSCREEN function only
                                ;       preserves BP.
                                ;       This is satisfactory for COBOL
                                ;       CALL modules, but may not be in
                                ;       the general case.
;*                                                                      *
;************************************************************************

_TEXT   ends

IXFUN   segment word public 'IXFUN_DATA'

            even
public      $$Function          ;   $$FUNCTION defines the start address for
$$Function  equ this byte       ;   the nodes to install.
IXFUN   ends                    ;   ADDFUN should be run outside any segments.

;************************************************************************
;*                                                                      *
;* !!!  We have one ADDFUN line for each entry point to be installed    *
;*                                                                      *
;*      Parameters are: 1   External name                               *
;*                      2   COBOL name (must be upper case)             *
;*                      3   Preparsing function name (optional)         *
;*                                 (See VioWrtTTY for an example.)      *
;*                      4   Words of parameters if CALL GATE is not     *
;*                                  same priviledge (optional)          *
;*                                 (See DosBeep for an example.         *
;*                                  Installed functions are accessed    *
;*                                  with a JMP instruction which cannot *
;*                                  be targeted on a CALL gate unless   *
;*                                  it is at the same priviledge level. *
;*                                  A JMP to a different level will     *
;*                                  cause an exception.                 *
;*                                  If you get this, simply add the     *
;*                                  fourth parameter and the ADDFUN     *
;*                                  macro will CALL rather than JMP     *
;*                                  to the routine.)                    *
;*                          If the target routine is guaranteed to      *
;*                                  preserve SI and DI (as all OS/2     *
;*                                  API functions do) you can make      *
;*                                  this parameter zero and make use    *
;*                                  of a small optimization in ADDFUN.  *
;*                                                                      *
;*      NB If parameter 3 is not required but parameter 4 is            *
;*          required, you should specify a null (or blank)              *
;*          parameter 3. (See DosBeep for an example of this.)          *
;*                                                                      *
;************************************************************************
includeall  %CALLFNS            ;   Include the specified files
;*                                                                      *
;************************************************************************

IXFUN   segment                 ;   Finish the node segment
                even            ;   on an even boundary
public          $$FunctionEnd   ;   $$FunctionEnd is the address after
$$FunctionEnd   equ this byte   ;   the last installed node.

;************************************************************************
;*                                                                      *
;* !!!  We can declare a message to be shown when the routines are      *
;*      installed.  (Zero length will show no message.)                 *
;*      We can also declare a message to be shown if a routine does     *
;*      not successfully install.                                       *
;*                                                                      *
;************************************************************************

;OKMessage is the Success message
public  $$OKMessage, $$OKMessageL

$$OKMessage     equ this byte
;
;The following message can be used to inform the user that the installation
;was successful, but it will come out on the Animator screen requiring a
;T(ext) R(efresh) to restore the Animator screen.
;
;message         <%CALLSET,<Calls - installed ok.>>
;
;$$OKMessageL    equ $ - $$OKMessage
$$OKMessageL    equ 0

;NotOKMessage is the Routine Failure message
public  $$NotOKMessage, $$NotOKMessageL

$$NotOKMessage  equ this byte
;
;The following message will be shown on the currently showing screen
;indicating any failures in installation.
;eg if the function MYFUNCTION is fails to install
;   MYFUNCTION not installed
;       will be displayed at the current cursor position.
;
message         < not installed.>
;
$$NotOKMessageL equ $ - $$NotOKMessage

;*                                                                      *
;************************************************************************

IXFUN   ends
        end
