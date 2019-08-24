;*****************************************************************************
;*
;*
;* THIS IS AN EXAMPLE OF USER CALL CODE SUPPLIED PURELY FOR GUIDANCE OF THE
;* USER TO ENABLE THE MECHANICS OF CALL CODE INSERTION TO BE BETTER UNDERSTOOD.
;*
;* THE CODE IS DESIGNED TO BE A USEFUL EXAMPLE OF CALL, AND IF IMPLEMENTED
;* WILL ALLOW THE COBOL PROGRAMMER TO CREATE 16 BIT BINARY QUANTITIES FROM
;* UP TO 5 ASCII DIGITS, AND VICE VERSA.  THE USE IS EXPLAINED IN MORE DETAIL
;* AT THE HEAD OF EACH ROUTINE.
;*
;* INTEL CORPORATION HAS TAKEN EVERY PRECAUTION TO ENSURE THE ACCURACY OF THESE
;* ROUTINES, BUT CANNOT BE HELD LIABLE IN ANY WAY FOR ANY ERRORS OR
;* OMISSIONS IN THEM.
;*
;*****************************************************************************
;*
;* THE MODULE MUST BE LOCATED AT THE ADDRESS SPECIFIED BY CONFIGURATOR
;* WHEN THE RTS IN WHICH THE CODE IS TO RESIDE WAS CONFIGURED. (SEE OPERATING
;* GUIDE, CHAPTER 4).
;*
BASE   EQU     04404H          ;REPLACE 04404H BY THE ADDRESS GIVEN
                                ;BY CONFIGURATOR.
;*
;*
        ORG     BASE            ;SET THE BASE ADDRESS
;*
;*****************************************************************************
;* 
;* NOW FOLLOWS THE CALL CODE IDENTIFICATION TABLE. THIS IS A TABLE OF
;* ADDRESSES OF THE ENTRY-POINTS TO THE ROUTINES, PRECEDED BY A BINARY
;* 8 BIT ITEM SPECIFYING THE HIGHEST AVAILABLE ROUTINE NUMBER
;*
;*****************************************************************************
;*
CALTOP: DB      MAXNO           ;HIGHEST AVAILABLE CALL ROUTINE.
        DW      0               ;CALL "00" (DOES NOT EXIST)
        DW      DECBIN          ;CALL "01" - DECIMAL ASCII TO BINARY
        DW      BINDEC          ;CALL "02" - BINARY TO DECIMAL ASCII
MAXNO  EQU     ($-CALTOP-3)/2  ;LET THE ASSEMBLER DO THE WORK.
;*
;*****************************************************************************
;*
;* NB. ALTHOUGH THE USE OF CALL "00" IN THE ABOVE EXAMPLE WOULD CAUSE
;* THE RTS TO ISSUE THE FOLLOWING ERROR:-
;*                  164 - CALL CODE DOES NOT EXIST
;* THE USER IS AT LIBERTY TO PROVIDE HIS OWN CODE, BY PLUGGING IN
;* THE APPROPRIATE ROUTINE ADDRESS.
;*
;* SIMILARLY, OTHER ROUTINES MAY BE ADDED BY INCREASING THE NUMBER
;* OF ADDRESSES SPECIFIED.  IF THESE ARE ADDED BEFORE THE MAXNO EQUATE,
;* THEN THE BYTE AT CALTOP WILL ALWAYS BE CORRECT
;*
;*
;*
;*
;
;
;
;
;
;
;
;*****************************************************************************
;*
;*ROUTINE:       DECBIN
;*
;*CALLING SEQUENCE: 
;*               CALL "01" USING PARA1 PARA2 PARA3.
;*
;*FUNCTION:      THIS ROUTINE CONVERTS A STRING OF DECIMAL (ASCII)
;*               DIGITS INTO A 16 BIT BINARY QUANTITY. IT IS VERY LOW LEVEL
;*               IN THAT IT EXPECTS A POSITIVE DECIMAL VALUE
;*
;*PARAMETERS:    PARA1 - ADDRESS OF LENGTH OF DECIMAL STRING
;*                       HELD AS 1 BYTE ASCII DIGIT (NOT CHECKED)
;*                       THIS ADDRESS WILL BE NO. 2 ON STACK
;*
;*               PARA2 - ADDRESS OF DECIMAL STRING
;*                       THIS ADDRESS WILL BE IN B,C ON ENTRY
;*
;*               PARA3 - ADDRESS OF RESULT AREA.
;*                       SPECIFIES A 2 BYTE AREA
;*                       THIS ADDRESS WILL BE IN D,E ON ENTRY
;*
;*VALUES RETURNED:       16 BIT RESULT IN PARA3
;*
;*
;*****************************************************************************
;*
DECBIN:
        POP     H               ;GET RETURN ADDRESS OFF STACK
        XTHL                    ;GET ADDRESS OF PARA1
                                ;PUTTING RETURN ADDRESS BACK.
;*
        MOV     A,M             ;PUT IT IN ACCUMULATOR
        ANI     0FH             ;CONVERT TO BINARY
 
        PUSH    D               ;SAVE ADDRESS OF RESULT
        PUSH    B               ;MOVE STRING REF
        POP     D               ;  INTO D,E
        LXI     H,0             ;HL = BINARY ACCUMULATOR
DEC10:
        PUSH    PSW             ;SAVE THE COUNT
        DAD     H               ;BINARY ACCUMULATOR *2
        MOV     B,H             ;  AND MOVE IT INTO B,C
        MOV     C,L             ;
        DAD     H               ;BINARY ACCUMULATOR *4
        DAD     H               ;                   *8
        DAD     B               ;         *8 + *2 = *10
                                ; (IE. 8X + 2X = 10X)
                                ;------------------------
        LDAX    D               ;GET THE DECIMAL CHAR
        INX     D
        ANI     0FH             ;CONVERT TO BINARY CHAR
        MVI     B,0H
        MOV     C,A
        DAD     B               ;ACC + CHAR
        POP     PSW
        DCR     A               ;KEEP COUNT
        JNZ     DEC10
;*
;*              NOW STORE RESULT IN USER'S AREA.
;*
        XCHG                    ;PUT RESULT IN D,E
        POP     H               ;GET ADDRESS OF RESULT AREA
        MOV     M,D             ;STORE MS BYTE
        INX     H
        MOV     M,E             ;STORE LS BYTE
        RET
        ;
;*****************************************************************************
;*
;*ROUTINE:       BINDEC
;*
;*CALLING SEQUENCE:
;*               CALL "02" USING PARA1 PARA2.
;*
;*FUNCTION:      TAKES THE BINARY QUANTITY ADDRESSED BY PARA1 AND CONVERTS
;*               IT INTO A 5 DIGIT DECIMAL (ASCII) NO. THE RESULT IS PLACED
;*               IN THE AREA SPECIFIED BY PARA2.
;*
;*PARAMETERS:    PARA1 = ADDRESS OF 16 BIT (2 BYTE) QUANTITY.
;*                       WILL BE IN REG B,C ON ENTRY
;*
;*               PARA2 = ADDRESS OF 5 BYTE RESULT AREA.
;*                       WILL BE IN REG D,E ON ENTRY
;*
;*VALUES RETURNED:
;*               5 DIGIT ASCII VALUE IN PARA2.
;*
;*****************************************************************************
BINDEC:
        PUSH    B               ;GET VALUE ADDR
        POP     H               ;  IN H,L
        MOV     B,M             ;VALUE
        INX     H               ;  IN
        MOV     C,M             ; B,C
        LXI     H,0             ;PUSH CONSTANTS
        PUSH    H               ;   ON TO
        LXI     H,-10           ;   STACK
        PUSH    H               ;   FOR USE
        LXI     H,-100          ;   DURING
;*
        PUSH    H               ;   BINARY TO DECIMAL CONVERSION.
        LXI     H,-1000
        PUSH    H
        LXI     H,-10000
        PUSH    H
;*                               ;D,E = ADDRESS OF RESULT FIELD
CN25:
        MVI     A,30H           ;SET TALLY TO ASCII ZERO
CN30:
        POP     H               ;GET THE CONSTANT
        PUSH    H               ;RESTORE IT
        DAD     B               ;SUBTRACT FROM SOURCE OP
        JNC     CN40            ;ITS GONE NEGATIVE
        INR     A               ;INC TALLY
        PUSH    H               ;REPLACE B,C WITH
        POP     B               ;   NEW RESULT
        JMP     CN30
CN40:
        POP     H               ;CLEAR CONSTANT OFF STACK
        STAX    D               ;STORE TALLY IN RESULT FIELD
        INX     D               ;INC RESULT ADDR POINTER
        POP     H               ;ANY MORE CONSTANTS ?
        MOV     A,L
        ORA     H
        JZ      CN50            ;NO - FINISH OFF
        PUSH    H               ;YES - RESTORE IT
        JMP     CN25
CN50:
        MOV     A,C             ;INSERT UNITS
        ADI     30H             ;CONVERT TO ASCII
        STAX    D
        RET                     ;RETURN
;*
;*
;*

        END
