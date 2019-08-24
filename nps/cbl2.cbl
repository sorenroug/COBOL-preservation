IDENTIFICATION DIVISION.

PROGRAM-ID.      CBL2.
AUTHOR.          EILEEN STRAUS.
DATE-WRITTEN.    09/18/80.

***************************************************     
*                                                 *
*  THIS PROGRAM PRODUCES A LIST OF NEW EMPLOYEES. *
*                                                 *
***************************************************

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.
    8080.
OBJECT-COMPUTER.
    8080.
INPUT-OUTPUT SECTION.

FILE-CONTROL.
    SELECT EMP-INPUT-FILE ASSIGN
    CDL2.FIL.
    SELECT EMP-REPORT-FILE ASSIGN
    PRL2.FIL.

DATA DIVISION.

FILE SECTION.

FD  EMP-INPUT-FILE
      RECORD 1 TO 80
      LABEL RECORDS
      OMITTED.
01  EMP-INPUT-RECORD.
    05  FILLER                            PIC X(8).
    05  DEPT-NUM-INPUT                    PIC XX.
    05  FILLER                            PIC X(3).
    05  NAME-INPUT.
        10  FIRST-LETTER-INPUT            PIC X.
        10  REST-OF-NAME-INPUT            PIC X(19).
    05  FILLER                            PIC X(15).
    05  SSN-INPUT.
        10  FST-3-SSN-INPUT               PIC X(3).
        10  MID-2-SSN-INPUT               PIC X(2).
        10  LST-4-SSN-INPUT               PIC X(4).
    05  FILLER                            PIC X(23).
FD  EMP-REPORT-FILE
      LABEL RECORDS
      OMITTED.
01  EMP-REPORT-LINE.
    05  FILLER                            PIC X(4).
    05  EMP-ID-NUM-REPORT.
        10  ID-FIRST-LETTER-REPORT        PIC X.
        10  ID-2-5-DIGITS-REPORT          PIC X(4).
        10  ID-DEPT-NUM-REPORT            PIC XX.
    05  FILLER                            PIC X(10).
    05  NAME-REPORT                       PIC X(20).
    05  FILLER                            PIC X(10).
    05  SSN-REPORT.
        10  FST-3-SSN-REPORT              PIC X(3).
        10  FILLER                        PIC X.
        10  MID-2-SSN-REPORT              PIC XX.
        10  FILLER                        PIC X.
        10  LST-4-SSN-REPORT              PIC X(4).
    05  FILLER                            PIC X(10).
    05  DEPT-NUM-REPORT                   PIC XX.
    05  FILLER                            PIC X(6).

WORKING-STORAGE SECTION.

01  PROGRAM-INDICATORS.
    05  MORE-RECORDS  PIC X(3) VALUE 'YES'.

PROCEDURE DIVISION.

**************************************************************
*                                                            *
* THIS PROGRAM READS THE EMPLOYEE INPUT RECORDS AND PRODUCES *
* A LIST OF NEW EMPLOYEES. IT IS ENTERED FROM THE OPERATING  *
* SYSTEM AND EXITS TO THE OPERATING SYSTEM.                  *
*                                                            *
**************************************************************
A10-CR-EMP-LIST.
    OPEN INPUT EMP-INPUT-FILE.
    OPEN OUTPUT EMP-REPORT-FILE.
    READ EMP-INPUT-FILE
         END
             MOVE 'NO ' TO MORE-RECORDS.
    PERFORM A20-FMT-PRT-RPT
          UNTIL MORE-RECORDS EQUAL 'NO '.
    CLOSE EMP-INPUT-FILE.
    CLOSE EMP-REPORT-FILE.
    STOP RUN.
A20-FMT-PRT-RPT.
    MOVE SPACE TO EMP-REPORT-LINE.
    MOVE FIRST-LETTER-INPUT TO ID-FIRST-LETTER-REPORT.
    MOVE LST-4-SSN-INPUT TO ID-2-5-DIGITS-REPORT.
    MOVE DEPT-NUM-INPUT TO ID-DEPT-NUM-REPORT.
    MOVE NAME-INPUT TO NAME-REPORT.
    MOVE FST-3-SSN-INPUT TO FST-3-SSN-REPORT.
    MOVE MID-2-SSN-INPUT TO MID-2-SSN-REPORT.
    MOVE LST-4-SSN-INPUT TO LST-4-SSN-REPORT.
    MOVE DEPT-NUM-INPUT TO DEPT-NUM-REPORT.
    WRITE EMP-REPORT-LINE
         AFTER ADVANCING 1.
    READ EMP-INPUT-FILE
          END
            MOVE 'NO ' TO MORE-RECORDS.
EOF
