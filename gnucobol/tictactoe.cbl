       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIC-TAC-TOE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BOARD.
           10  S OCCURS 9             PIC S9.
       01  BOARD2D REDEFINES BOARD.
           10  ROW OCCURS 3.
               20 CELL OCCURS 3 PIC S9.
       01  ROW-DISPLAY.
           10  CELLD OCCURS 3.
             15 FILLER PIC X.
             15 CELL-DISPLAY PIC X.
             15 FILLER PIC X.
             15 DIVIDER PIC X.
       77  I   PIC 99.
       77  J   PIC 9.
       77  K   PIC 9.
       77  INX PIC 9.
       77  M   PIC 9.
      * If G = -1 then computer has just made move
       77  G   PIC S9.
       77  H   PIC S9.
       01  COMP-MARK                   CONSTANT AS -1.
       01  UNOCCUPIED                  CONSTANT AS 0.
       01  OPPO-MARK                   CONSTANT AS 1.
      * C = Player choice of O or X
       77  C   PIC X.
       77  P   PIC X.
       77  Q   PIC X.

       PROCEDURE DIVISION.
       TICTACTOE.
           MOVE ZEROS TO BOARD.
           MOVE "   !   !    " TO ROW-DISPLAY.
           DISPLAY "              TIC-TAC-TOE".
           DISPLAY "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY".
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY "THE BOARD IS NUMBERED:".
           DISPLAY " 1  2  3".
           DISPLAY " 4  5  6".
           DISPLAY " 7  8  9".
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY SPACE.
           DISPLAY "DO YOU WANT 'X' OR 'O'?" WITH NO ADVANCING.
           ACCEPT C.
           IF C = "X" OR "x" GO TO PLAYER-IS-X.
           MOVE "O" TO P.
           MOVE "X" TO Q.
       NEXT-ROUND.
           MOVE COMP-MARK TO G.
           MOVE OPPO-MARK TO H.
      * Is center occupied?
           IF S(5) = UNOCCUPIED
               MOVE COMP-MARK TO S (5)
               GO TO DISP-COMP-MOVE.
      * Is center mine?
           IF S(5) <> OPPO-MARK GO TO LIN106.
      * Is top left occupied?
           IF S(1) <> UNOCCUPIED GO TO LIN110.
           MOVE COMP-MARK TO S(1).
           GO TO DISP-COMP-MOVE.
       LIN106.
           IF S(2) = OPPO-MARK AND S(1) = UNOCCUPIED
                   OR S(4) = OPPO-MARK AND S(1) = UNOCCUPIED
                   GO TO TAKE-CELL-1.
           IF S(6) = OPPO-MARK AND S(9) = UNOCCUPIED GO TO TAKE-CELL-9.
           IF S(8) = OPPO-MARK AND S(9) = UNOCCUPIED GO TO TAKE-CELL-9.
       LIN110.
           IF G = OPPO-MARK GO TO LIN112.
           GO TO LIN118.
       LIN112.
           MOVE M TO INX.
           SUBTRACT 1 FROM INX.
      * Calculate row
           DIVIDE 3 INTO INX.
           MULTIPLY 3 BY INX.
           ADD 1 TO INX.
           MOVE INX TO J.
           IF INX = M THEN MOVE 1 TO K.
           ADD 1 TO INX.
           IF INX = M THEN MOVE 2 TO K.
           ADD 1 TO INX.
           IF INX = M THEN MOVE 3 TO K.
           GO TO LIN120.

       LIN118.
           MOVE 1 TO J.
       LIN119.
           MOVE 1 TO K.
       LIN120.
           IF S(J) <> G GO TO LIN130.
           IF S(J + 2) <> G GO TO LIN135.
           IF S(J + 1) <> UNOCCUPIED GO TO LIN150.
           MOVE COMP-MARK TO S(J + 1).
           GO TO DISP-COMP-MOVE.
       LIN130.
           IF S(J) = H GO TO LIN150.
           IF S(J + 1) <> G GO TO LIN150.
           IF S(J + 2) <> G GO TO LIN150.
           MOVE COMP-MARK TO S(J).
           GO TO DISP-COMP-MOVE.
      * Take last in row
       LIN135.
           IF S(J + 1) <> G GO TO LIN150.
           IF S(J + 2) <> UNOCCUPIED GO TO LIN150.
           MOVE COMP-MARK TO S(J + 2).
           GO TO DISP-COMP-MOVE.
      * Take middle in column if top and bottom not taken
       LIN150.
           IF S(K) <> G GO TO LIN160.
           IF S(K + 6) <> G GO TO LIN165.
           IF S(K + 3) <> UNOCCUPIED GO TO LIN170.
           MOVE COMP-MARK TO S(K + 3).
           GO TO DISP-COMP-MOVE.
      * Take top in column
       LIN160.
           IF S(K) = H GO TO LIN170.
           IF S(K + 3) <> G GO TO LIN170.
           IF S(K + 6) <> G GO TO LIN170.
           MOVE COMP-MARK TO S(K).
           GO TO DISP-COMP-MOVE.
       LIN165.
           IF S(K + 3) <> G GO TO LIN170.
           IF S(K + 6) <> UNOCCUPIED GO TO LIN170.
           MOVE COMP-MARK TO S(K + 6).
           GO TO DISP-COMP-MOVE.
       LIN170.
           GO TO LIN450.
       LIN171.
           IF S(3) = G AND S(7) = UNOCCUPIED
               MOVE COMP-MARK TO S(7)
               GO TO DISP-COMP-MOVE.
           IF S(9) = G AND S(1) = UNOCCUPIED GO TO TAKE-CELL-1.
           IF S(7) = G AND S(3) = UNOCCUPIED GO TO TAKE-CELL-3.
           IF S(9) = UNOCCUPIED AND S(1) = G GO TO TAKE-CELL-9.
       LIN175.
           IF G = COMP-MARK THEN
               MOVE OPPO-MARK TO G
               MOVE COMP-MARK TO H
               GO TO LIN110.
           IF S(9) = OPPO-MARK AND S(3) = UNOCCUPIED GO TO MAYBE-CELL-3.
      * Take first non-empty cell larger than 1
       FIND-FREE.
           MOVE 2 TO I.
       NEXT-FREE-CELL.
           IF I < 10
               IF S(I) <> UNOCCUPIED
                   ADD 1 TO I
                   GO TO NEXT-FREE-CELL
               ELSE
                   MOVE COMP-MARK TO S(I)
                   GO TO DISP-COMP-MOVE.
       TAKE-CELL-1.
           MOVE COMP-MARK TO S(1).
           GO TO DISP-COMP-MOVE.
       MAYBE-CELL-3.
           IF S(1) = OPPO-MARK GO TO FIND-FREE.
       TAKE-CELL-3.
           MOVE COMP-MARK TO S(3).
           GO TO DISP-COMP-MOVE.
       TAKE-CELL-9.
           MOVE COMP-MARK TO S(9).

       DISP-COMP-MOVE.
           DISPLAY SPACE.
           DISPLAY "THE COMPUTER MOVES TO...".
           PERFORM PRINT-BOARD THRU PRINT-BOARD-EXIT.
           GO TO ASK-PLAYER.
       LIN450.
           IF G=1 GO TO LIN465.
           IF J=7 AND K=3 GO TO LIN465.
           ADD 1 TO K.
           IF K NOT > 3 GO TO LIN120.
           ADD 3 TO J.
           IF J NOT > 7 GO TO LIN119.
       LIN465.
           IF S(5) = G GO TO LIN171.
           GO TO LIN175.

       PLAYER-IS-X.
           MOVE "X" TO P.
           MOVE "O" TO Q.
       ASK-PLAYER.
           DISPLAY SPACE.
           DISPLAY "WHERE DO YOU MOVE? (0 = END)" WITH NO ADVANCING.
           ACCEPT M.
           IF M = 0 THEN
               DISPLAY "THANKS FOR THE GAME."
               GO TO END-GAME.
           IF S(M) = UNOCCUPIED GO TO MARK-CHOICE.
       ILLEGAL-MOVE.
           DISPLAY "THAT SQUARE IS OCCUPIED.".
           DISPLAY SPACE.
           DISPLAY SPACE.
           GO TO ASK-PLAYER.
      * Set player's marker in cell
       MARK-CHOICE.
           MOVE OPPO-MARK TO G.
           MOVE OPPO-MARK TO S(M).
           PERFORM PRINT-BOARD THRU PRINT-BOARD-EXIT.
           GO TO NEXT-ROUND.

       PRINT-BOARD.
           DISPLAY SPACE.
           PERFORM PRINT-ROW VARYING J FROM 1 BY 1 UNTIL J > 3.
           GO TO BOARD-STATUS.

       PRINT-ROW.
           PERFORM MARK-CELL VARYING I FROM 1 BY 1 UNTIL I > 3.
           DISPLAY ROW-DISPLAY.
           IF J < 3 DISPLAY "---+---+---".

       MARK-CELL.
           IF CELL(J,I) = COMP-MARK
                   MOVE Q TO CELL-DISPLAY(I).
           IF CELL(J,I) = UNOCCUPIED
                   MOVE " " TO CELL-DISPLAY(I).
           IF CELL(J,I) = OPPO-MARK
                   MOVE P TO CELL-DISPLAY(I).

      * Check board status
       BOARD-STATUS.
           DISPLAY SPACE.
           DISPLAY SPACE.
           MOVE 1 TO I.
      * Check horizontally
       NEXT-ROW.
           IF S(I) <> S(I + 1) GO TO SKIP-ROW.
           IF S(I) <> S(I + 2) GO TO SKIP-ROW.
           IF S(I) = COMP-MARK GO TO COMPUTER-WIN.
           IF S(I) = OPPO-MARK GO TO PLAYER-WIN.
       SKIP-ROW.
           ADD 3 TO I.
           IF I NOT > 7 GO TO NEXT-ROW.

      * Check vertically
           MOVE 1 TO I.
       NEXT-COL.
           IF S(I) <> S(I + 3) GO TO SKIP-COL.
           IF S(I) <> S(I + 6) GO TO SKIP-COL.
           IF S(I) = COMP-MARK GO TO COMPUTER-WIN.
           IF S(I) = OPPO-MARK GO TO PLAYER-WIN.
       SKIP-COL.
           ADD 1 TO I.
           IF I NOT > 3 GO TO NEXT-COL.

      * Same markers diagonally?
       CHECK-DIAG.
           IF S(5) <> G GO TO CHECK-ALL-FILLED.
           IF S(1) = G AND S(9) = G GO TO WIN-DIAGONAL.
           IF S(3) = G AND S(7) = G GO TO WIN-DIAGONAL.

      * Any unfilled cells?
       CHECK-ALL-FILLED.
           MOVE 1 TO I.
       CHECK-NEXT-CELL.
           IF S(I) = UNOCCUPIED GO TO PRINT-BOARD-EXIT.
           ADD 1 TO I
           IF I NOT > 9 GO TO CHECK-NEXT-CELL.
           DISPLAY "IT'S A DRAW. THANK YOU.".
           GO TO END-GAME.
      * There are still empty cells.
       PRINT-BOARD-EXIT.
           EXIT.

       WIN-DIAGONAL.
           IF G = COMP-MARK GO TO COMPUTER-WIN.
       PLAYER-WIN.
           DISPLAY "YOU BEAT ME!! GOOD GAME.".
           GO TO END-GAME.
       COMPUTER-WIN.
           DISPLAY "I WIN, TURKEY!!!".
       END-GAME.
           STOP RUN.
