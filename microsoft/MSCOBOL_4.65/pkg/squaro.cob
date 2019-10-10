         IDENTIFICATION DIVISION.
         PROGRAM-ID.  SQUAROOT.
      *  REMARKS. CALCULATIONS BY NEWTON-RAPHSON METHOD.
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         77  NUM,  PIC 9(7)V99.
         01 NUMBERS.
               02 Z PIC 99999V9(13).
               02 A PIC 9(7)V9(11).
               02 OLD-ROOT PIC S9(10)V9(7).
               02 ROOT PIC S9(10)V9(7).
               02 TEN-THOU PIC S99999 VALUE 10000.
               02 COUNTERX PIC S999.
         01 FLAG PIC X.
         01  RESULTS.
               8 FILLER PIC X(15) VALUE 'SQUARE ROOT OF '.
               8  A-OUT  PIC Z(7).9(9).
               8 FILLER PIC XXXX VALUE ' IS '.
               8 ANSWER PIC Z(7).9(9).
               8 FILLER PIC X(15) VALUE ' # ITERATIONS ='.
               8 IT PIC 99.
               8 FILLER PIC X VALUE '.'.
      /
         PROCEDURE DIVISION.
         P.  DISPLAY 'KEY IN "A" AS 9(7)V9(11):'.
             ACCEPT A.
         R.  IF A IS NOT NUMERIC
               DISPLAY 'ILLEGAL DATA' GO TO P.
             IF A NOT GREATER THAN 0 DISPLAY '0 IS EOJ.'
             DISPLAY SPACE STOP RUN.
         S.  IF A LESS THAN 1 COMPUTE A = A * TEN-THOU
               MOVE '*' TO FLAG ELSE MOVE SPACE TO FLAG.
         T.  DIVIDE A BY 2 GIVING OLD-ROOT
               MOVE ZERO TO COUNTERX.
         CALCULATION.
               COMPUTE ROOT = (OLD-ROOT + A / OLD-ROOT) / 2
      *        COMPUTE Z = 1 - ROOT * ROOT / A
               COMPUTE Z = ROOT * ROOT
               COMPUTE Z = Z / A
               SUBTRACT Z FROM 1 GIVING Z
      *        EXHIBIT NAMED ROOT OLD-ROOT Z
               ADD  1  TO  COUNTERX.
               IF COUNTERX > 20 DISPLAY 'CONVERGENCE NOT ATTAINED WITHIN
      -    ' 20 TERMS ***'   GO TO DO-OUTPUT.
               IF Z <   .00001  GO TO DO-OUTPUT.
               MOVE ROOT TO OLD-ROOT  GO TO CALCULATION.
         DO-OUTPUT.
               IF FLAG NOT = SPACE COMPUTE A = A / TEN-THOU
                 COMPUTE ROOT = ROOT / 100.
               MOVE A TO A-OUT
               MOVE COUNTERX TO IT
               MOVE ROOT TO ANSWER DISPLAY RESULTS
               GO TO P.
