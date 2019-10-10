       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKERS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MS-COBOL-3.
       OBJECT-COMPUTER. MS-DOS.
       SPECIAL-NAMES.
           CONSOLE IS CRT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ABS1   PIC 9.
       77  ABS2   PIC 9.
       77  X-KING PIC S9 VALUE -2.
       77  X-MAN  PIC S9 VALUE -1.
       77  EMPTY  PIC S9 VALUE 0.
       77  O-MAN  PIC S9 VALUE 1.
       77  O-KING PIC S9 VALUE 2.
      * Dimension of S is base 0 in BASIC
       01  BOARD.
           10  ROW OCCURS 8.
               20 S OCCURS 8           PIC S9.
       01  INITVALS PIC X(32)
           VALUE "+1+0+1+0+0+0-1+0+0+1+0+0+0-1+0-1".
       01  INITDATA REDEFINES INITVALS.
           10 VAL OCCURS 16            PIC S9 SIGN IS LEADING SEPARATE.
       77  A                           PIC S9.
       77  A1                          PIC 9.
       77  B                           PIC S9.
       77  B1                          PIC 9.
       77  C                           PIC S9.
       77  E                           PIC S9.
       77  G                           PIC S9.
       77  H                           PIC S9.
       77  I                           PIC S99.
       77  J                           PIC S99.
       77  L                           PIC 9.
       77  M                           PIC 9.
       77  Q                           PIC S99 VALUE 0.
       77  P                           PIC XXX.
       77  R0                          PIC S99.
       77  R1                          PIC S99.
       77  R2                          PIC S99.
       77  R3                          PIC S99.
       77  R4                          PIC S99.
       77  T                           PIC 9 VALUE 0.
       77  U                           PIC S99.
       77  V                           PIC S99.
       77  X                           PIC 99.
       77  Y                           PIC 9.
       77  Z                           PIC 9 VALUE 0.
       77  TMP1                        PIC S99.
       77  TMP2                        PIC S99.
       77  TMP3                        PIC S99.
       77  TMP4                        PIC S99.
       77  CRTCOL                      PIC 99.
       77  CRTLIN                      PIC 99.
       01 X-LEGEND PIC X(44)
            VALUE "+   1    2    3    4    5    6    7    8   +".
       01  EXTRA-TO-MASK.
           05  TO-LBL                  PIC XXX VALUE "TO ".
           05  TO-X                    PIC 9.
           05  TO-COMMA                PIC X VALUE ",".
           05  TO-Y                    PIC 9.
       01 PLUS-TO-ENTRY.
           05 PLUS-LBL                 PIC X(7) VALUE "+TO 0,0".

       SCREEN SECTION.
       01 INTRO.
           05 VALUE "CHECKERS" LINE 1 COLUMN 36 BLANK SCREEN.
           05 VALUE "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
                LINE 2 COLUMN 20.
           05 VALUE "THIS IS THE GAME OF CHECKERS.  THE COMPUTER IS X,"
                LINE 4 COLUMN 16.
           05 VALUE "AND YOU ARE O.  THE COMPUTER WILL MOVE FIRST."
                LINE PLUS 1 COLUMN 16.
           05 VALUE "SQUARES ARE REFERRED TO BY A COORDINATE SYSTEM,"
                LINE PLUS 1 COLUMN 16.
           05 "WHERE (1,1) IS THE LOWER LEFT CORNER."
                LINE PLUS 1 COLUMN 16.
           05 VALUE "(1,8) IS THE UPPER LEFT CORNER"
                LINE PLUS 1 COLUMN 16.
           05 VALUE "(8,1) IS THE LOWER RIGHT CORNER"
                LINE PLUS 1 COLUMN 16.
           05 VALUE "(8,8) IS THE UPPER RIGHT CORNER"
                LINE PLUS 1 COLUMN 16.
           05 VALUE "THE COMPUTER WILL TYPE '+TO' WHEN YOU HAVE ANOTHER"
                LINE PLUS 1 COLUMN 16.
           05 VALUE "JUMP. TYPE TWO ZEROES IF YOU CANNOT JUMP."
                LINE PLUS 1 COLUMN 16.
           05 VALUE "READY TO PLAY (Y/N)?"
                LINE PLUS 2 COLUMN 16.
       01  MOVE-MASK.
           05  VALUE "COMPUTER MOVES FROM " LINE 2 COLUMN 1.
           05  FROM-X                  PIC 9 FROM R1.
           05  VALUE ",".
           05  FROM-Y                  PIC 9 FROM R2.
           05  VALUE " TO ".
           05  TO-X                    PIC 9 FROM R3.
           05  VALUE ",".
           05  TO-Y                    PIC 9 FROM R4.
       01 FROM-ENTRY AUTO.
           05 VALUE "ENTER FROM: " LINE 23 COLUMN 1.
           05 X-INPUT                  PIC 9 USING E.
           05 VALUE ",".
           05 Y-INPUT                  PIC 9 USING H.
       01 TO-ENTRY AUTO.
           05 VALUE " TO " LINE 23 COLUMN 16.
           05 X-INPUT                  PIC 9 USING A.
           05 VALUE ",".
           05 Y-INPUT                  PIC 9 USING B.

       01 MOVE-SPACER VALUE " "      LINE 2 COLUMN 31 BLANK LINE.
       01 ENTRY-SPACER VALUE " "      LINE 23 COLUMN 1 BLANK LINE.
       01 CLEAR-MSG-LINE   VALUE "            "
               LINE 24 COLUMN 10.
       01 MSG-ILLEGAL-MOVE VALUE "ILLEGAL MOVE" LINE 24 COLUMN 10.
       01 MSG-I-WIN VALUE "I WIN."     LINE 24 COLUMN 10.
       01 MSG-YOU-WIN VALUE "YOU WIN." LINE 24 COLUMN 10.

       PROCEDURE DIVISION.
       MAIN.
000005     DISPLAY INTRO
           ACCEPT P AT 1437
           IF P = "N" OR "n" STOP RUN.
000065     DISPLAY SPACE
000080*    DIM R(5),S(7,7)
           MOVE -1 TO G.
           MOVE -99 TO R0.
000090*    DATA 1,0,1,0,0,0,-1,0,0,1,0,0,0,-1,0,-1,15
           MOVE 1 TO I.
000120     PERFORM LOAD-CELL VARYING X FROM 1 BY 1 UNTIL X > 8
               AFTER Y FROM 1 BY 1 UNTIL Y > 8.

      * Computer calculates next move
000230 LINE0230.
           PERFORM CHECK-JUMPS THRU CHECK-EXIT
               VARYING X FROM 1 BY 1 UNTIL X > 8
                   AFTER Y FROM 1 BY 1 UNTIL Y > 8.
           GO TO LINE1140.

       CHECK-JUMPS.
           IF S (X, Y) > -1 GO TO CHECK-EXIT.
000310     IF S (X, Y) = X-MAN 
               PERFORM CHECK-FOR-MAN
                   VARYING A FROM -1 BY 2 UNTIL A > 1.
000330     IF S (X, Y) = X-KING 
               PERFORM CHECK-FOR-KING
                   VARYING A FROM -1 BY 2 UNTIL A > 1.
       CHECK-EXIT.
           EXIT.

       CHECK-FOR-MAN.
           MOVE G TO B
           PERFORM LINE0650 THRU EXIT0650.
       CHECK-FOR-KING.
           PERFORM LINE0650 THRU EXIT0650
               VARYING B FROM -1 BY 2 UNTIL B > 1.

000650 LINE0650.
           ADD X, A GIVING U
           ADD Y, B GIVING V
           IF U < 1 OR U > 8 OR V < 1 OR V > 8 GO TO EXIT0650.
000740     IF S (U, V) = EMPTY 
               PERFORM LINE0910
               GO TO EXIT0650.
000770     IF S (U, V) < 0 GO TO EXIT0650.
000790     ADD A TO U.
           ADD B TO V.
           IF U < 1 OR V < 1 OR U > 8 OR V > 8 GO TO EXIT0650.
000850     IF S (U, V) = EMPTY PERFORM LINE0910.
       EXIT0650.
           EXIT.

000910 LINE0910.
           IF V = 1 AND S (X, Y) = X-MAN ADD 2 TO Q.
           SUBTRACT V FROM Y GIVING ABS1 ON SIZE ERROR
               SUBTRACT Y FROM V GIVING ABS1.
000920     IF ABS1 = 2 ADD 5 TO Q.
000960     IF Y = 8 SUBTRACT 2 FROM Q.
000980     IF U = 1 OR U = 8 ADD 1 TO Q.
001030     PERFORM LINE1120 THRU EXIT1120
               VARYING C FROM -1 BY 2 UNTIL C > 1.
           IF Q > R0 
               MOVE Q TO R0
               MOVE X TO R1
               MOVE Y TO R2
               MOVE U TO R3
               MOVE V TO R4.
001100     MOVE 0 TO Q.

       LINE1120.
           ADD U, C GIVING TMP1
           ADD V, G GIVING TMP2
           IF TMP1 < 1 OR TMP1 > 8 OR TMP2 < 1 
               GO TO EXIT1120.
001035     IF S (TMP1, TMP2) < 0 
               ADD 1 TO Q
               GO TO EXIT1120.
           COMPUTE TMP3 = U - C
           COMPUTE TMP4 = V - G
001040     IF TMP3 < 1 OR TMP3 > 8 OR TMP4 > 8 
               GO TO EXIT1120.
001045     IF S (TMP1, TMP2) > 0
             AND (S (TMP3, TMP4) = 0 OR (TMP3 = X AND TMP4 = Y))
               SUBTRACT 2 FROM Q.
       EXIT1120.
           EXIT.

      * Display computer move
001140 LINE1140.
           IF R0 = -99 GO TO LINE1880.
           DISPLAY MOVE-SPACER
001230     DISPLAY MOVE-MASK
           MOVE -99 TO R0
           MOVE 32 TO CRTCOL.
001240 LINE1240.
           IF R4 = 1 
               MOVE X-KING TO S (R3, R4)
           ELSE
001250         MOVE S (R1, R2) TO S (R3, R4).
001310     MOVE EMPTY TO S (R1, R2)
           SUBTRACT R1 FROM R3 GIVING ABS1 ON SIZE ERROR
               SUBTRACT R3 FROM R1 GIVING ABS1.
           IF ABS1 NOT = 2 GO TO LINE1420.
           COMPUTE TMP1 = (R1 + R3) / 2
           COMPUTE TMP2 = (R2 + R4) / 2
001330     MOVE EMPTY TO S (TMP1, TMP2).
001340     MOVE R3 TO X.
           MOVE R4 TO Y.
           IF S (X, Y) = X-MAN 
               MOVE -2 TO B
               PERFORM LINE1370 THRU EXIT1370
                   VARYING A FROM -2 BY 4 UNTIL A > 2
           ELSE
001350         IF S (X, Y) = X-KING 
                   PERFORM LINE1370 THRU EXIT1370
                   VARYING A FROM -2 BY 4 UNTIL A > 2
001360                  AFTER B FROM -2 BY 4 UNTIL B > 2.
           IF R0 NOT = -99 
               MOVE R3 TO TO-X OF EXTRA-TO-MASK
               MOVE R4 TO TO-Y OF EXTRA-TO-MASK
               DISPLAY EXTRA-TO-MASK AT LINE 2 COLUMN CRTCOL
               ADD 7 TO CRTCOL
               MOVE -99 TO R0
               GO TO LINE1240.
001365     GO TO LINE1420.
      * See if there is a piece to jump over.
001370 LINE1370.
           ADD X, A GIVING U
           ADD Y, B GIVING V
           IF U<1 OR U>8 OR V<1 OR V > 8 GO TO EXIT1370.
           COMPUTE TMP1 = X + A / 2
           COMPUTE TMP2 = Y + B / 2
001380     IF S (U, V) = EMPTY AND S (TMP1, TMP2) > 0 
               PERFORM LINE0910.
       EXIT1370.
           EXIT.
      * Display board
001420 LINE1420.
           DISPLAY X-LEGEND AT 0419 WITH REVERSE-VIDEO
           MOVE 4 TO CRTLIN
           PERFORM DISP-ROW VARYING Y FROM 8 BY -1 UNTIL Y < 1.
           DISPLAY X-LEGEND AT 2119 WITH REVERSE-VIDEO.
      * Check if one player has no pieces left
001552     PERFORM TEST-CELL VARYING L FROM 1 BY 1 UNTIL L > 8
001554         AFTER M FROM 1 BY 1 UNTIL M > 8.
001564     IF Z NOT = 1 GO TO LINE1885.
001566     IF T NOT = 1 GO TO LINE1880.
001570     MOVE 0 TO Z
           MOVE 0 TO T.
           DISPLAY CLEAR-MSG-LINE.
      * Ask for player move
001590 LINE1590.
           DISPLAY ENTRY-SPACER
           MOVE 0 TO E, H
           DISPLAY FROM-ENTRY
           ACCEPT FROM-ENTRY
           IF E = 0 STOP RUN.
           MOVE E TO X.
           MOVE H TO Y.
           IF S (X, Y) NOT > 0
               DISPLAY MSG-ILLEGAL-MOVE
               GO TO LINE1590.
           DISPLAY CLEAR-MSG-LINE.
001670 LINE1670.
           MOVE 0 TO A, B
           DISPLAY TO-ENTRY
           ACCEPT TO-ENTRY
           IF A = 0 GO TO LINE1590.
           MOVE A TO X
           MOVE B TO Y
           SUBTRACT E FROM A GIVING ABS1 ON SIZE ERROR
               SUBTRACT A FROM E GIVING ABS1.
           SUBTRACT B FROM H GIVING ABS2 ON SIZE ERROR
               SUBTRACT H FROM B GIVING ABS2.
001680     IF S (X, Y) = EMPTY AND ABS1 NOT > 2 AND ABS1 = ABS2
               NEXT SENTENCE
           ELSE
001690         DISPLAY MSG-ILLEGAL-MOVE
               GO TO LINE1670.
001700     MOVE 24 TO CRTCOL.
001750 LINE1750.
           MOVE S (E, H) TO S (A, B)
           MOVE EMPTY TO S (E, H)
           SUBTRACT E FROM A GIVING ABS1 ON SIZE ERROR
               SUBTRACT A FROM E GIVING ABS1.
           IF ABS1 NOT = 2 GO TO LINE1810.
      * Erase jumped-over piece
           COMPUTE TMP1 = (E + A) / 2
           COMPUTE TMP2 = (H + B) / 2
001800     MOVE EMPTY TO S (TMP1, TMP2).
001802 LINE1802.
      * Player jumped. Ask for second move
           DISPLAY CLEAR-MSG-LINE
           MOVE 0 TO A1, B1
           DISPLAY PLUS-TO-ENTRY AT LINE 23 COLUMN CRTCOL
           ADD 4 TO CRTCOL
           ACCEPT A1 WITH AUTO-SKIP AT LINE 23 COLUMN CRTCOL
           ADD 2 TO CRTCOL
           ACCEPT B1 WITH AUTO-SKIP AT LINE 23 COLUMN CRTCOL
           ADD 2 TO CRTCOL
           IF A1 < 1 GO TO LINE1810.
           SUBTRACT A FROM A1 GIVING ABS1 ON SIZE ERROR
               SUBTRACT A1 FROM A GIVING ABS1.
           SUBTRACT B FROM B1 GIVING ABS2 ON SIZE ERROR
               SUBTRACT B1 FROM B GIVING ABS2.
001804     IF S (A1, B1) NOT = EMPTY
                 OR ABS1 NOT = 2 OR ABS2 NOT = 2
                GO TO LINE1802.
001806     MOVE A TO E.
           MOVE B TO H.
           MOVE A1 TO A.
           MOVE B1 TO B.
           GO TO LINE1750.
001810 LINE1810.
           DISPLAY "OK" AT LINE 23 COLUMN CRTCOL
           IF B = 8 MOVE O-KING TO S (A, B).
001830     GO TO LINE0230.
001880 LINE1880.
           DISPLAY MSG-YOU-WIN
           STOP RUN.
001885 LINE1885.
           DISPLAY MSG-I-WIN
           STOP RUN.

       LOAD-CELL.
           MOVE VAL(I) TO S (X, Y)
           ADD 1 TO I
           IF I > 16 MOVE 1 TO I.

       DISP-ROW.
           MULTIPLY Y BY 2 GIVING J
           SUBTRACT J FROM 21 GIVING CRTLIN
           DISPLAY Y AT LINE CRTLIN COLUMN 19
                     WITH REVERSE-VIDEO
           PERFORM DISP-CELL
               VARYING X FROM 1 BY 1 UNTIL X > 8.
           ADD 4 TO CRTCOL
           DISPLAY Y AT LINE CRTLIN COLUMN CRTCOL
                     WITH REVERSE-VIDEO
           ADD 1 TO CRTLIN
           DISPLAY " " AT LINE CRTLIN COLUMN 19
                     WITH REVERSE-VIDEO
           DISPLAY " " AT LINE CRTLIN COLUMN CRTCOL
                     WITH REVERSE-VIDEO.

       TEST-CELL.
001556     IF S (L, M) = O-MAN OR S (L, M) = O-KING 
               MOVE 1 TO Z.
001558     IF S (L, M) = X-MAN OR S (L, M) = X-KING 
               MOVE 1 TO T.

       DISP-CELL.
           MULTIPLY X BY 5 GIVING CRTCOL
           ADD 18 TO CRTCOL
001430     IF S (X, Y) = EMPTY DISPLAY ". "
                 AT LINE CRTLIN COLUMN CRTCOL.
001470     IF S (X, Y) = O-MAN DISPLAY "O "
                 AT LINE CRTLIN COLUMN CRTCOL.
001490     IF S (X, Y) = X-MAN DISPLAY "X "
                 AT LINE CRTLIN COLUMN CRTCOL.
001510     IF S (X, Y) = X-KING DISPLAY "X*"
                 AT LINE CRTLIN COLUMN CRTCOL.
001530     IF S (X, Y) = O-KING DISPLAY "O*"
                 AT LINE CRTLIN COLUMN CRTCOL.
