       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITERATIVE-TOWERS-OF-HANOI.
       AUTHOR. SOREN ROUG.
       DATE-WRITTEN. 2019-08-07.
      * Er's LLHanoi Hanoi Tower loop less algorithm
      * http://hanoitower.mkolar.org/algo.html
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LINUX.
       OBJECT-COMPUTER. KAYPRO4.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  NUM-DISKS                   PIC 99 VALUE 5.
       77  N1                          PIC 99 USAGE COMP.
       77  TO-PEG                      PIC 99 USAGE COMP.
       77  MOD3                        PIC 99 USAGE COMP.
       77  I                           PIC 99 USAGE COMP.
       77  IMASK1                      PIC 99 USAGE COMP.
       77  DIR                         PIC 99 USAGE COMP.
       01  STACKNUMS.
           10  D OCCURS 20             PIC 99 USAGE COMP.
       01  GAMESET.
           10  S OCCURS 20             PIC 99 USAGE COMP.
       PROCEDURE DIVISION.
       HANOI.
           DISPLAY 'TOWERS OF HANOI PUZZLE WITH ' NUM-DISKS ' DISKS.'.
           MOVE NUM-DISKS TO N1.
           ADD 1 TO N1.
           MOVE 1 TO I.
           PERFORM INIT-PUZZLE UNTIL I GREATER N1.
           MOVE NUM-DISKS TO DIR.
           MOVE NUM-DISKS TO I.
           DIVIDE 2 INTO I.
           MULTIPLY 2 BY I.
           SUBTRACT I FROM DIR.
           PERFORM MOVE-DISK THRU MOVE-END UNTIL I EQUAL N1.
           DISPLAY 'TOWERS OF HANOI PUZZLE COMPLETED!'.
           STOP RUN.

       INIT-PUZZLE.
           MOVE 1 TO D (I).
           MOVE I TO S (I).
           ADD 1 TO I.

       MOVE-DISK.
           MOVE S (1) TO I.
           IF I EQUAL N1
               GO MOVE-END
           END-IF.
           MOVE I TO IMASK1.
           DIVIDE 2 INTO IMASK1.
           MULTIPLY 2 BY IMASK1.
           MOVE D (I) TO TO-PEG.
           IF I NOT EQUAL IMASK1
               ADD DIR TO TO-PEG
           ELSE
               ADD 1 TO TO-PEG
               SUBTRACT DIR FROM TO-PEG
           END-IF.
           MOVE TO-PEG TO MOD3.
           DIVIDE 3 INTO MOD3.
           MULTIPLY 3 BY MOD3.
           SUBTRACT MOD3 FROM TO-PEG.
           ADD 1 TO TO-PEG.
           DISPLAY 'MOVE DISC ' I ' FROM ' D (I) ' TO ' TO-PEG.
           MOVE TO-PEG TO D (I).
           MOVE 1 TO S (1).
           MOVE I TO TO-PEG.
           ADD 1 TO TO-PEG.
           MOVE S (TO-PEG) TO S (I).
           MOVE I TO S (TO-PEG).
           ADD 1 TO S (TO-PEG).
       MOVE-END.
           EXIT.
