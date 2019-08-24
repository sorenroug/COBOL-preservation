 IDENTIFICATION DIVISION.
 PROGRAM-ID.
     DEMO.
************************************************
*                                              *
* THIS IS A COBOL PROGRAM WHICH DEMONSTRATES   *
* THE FORWARD REFERENCE MECHANISMS IN NPS      *
* MICRO-COBOL.                                 *
*                                              *
************************************************

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.
     8080.
 OBJECT-COMPUTER.
     8080.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT PRINT-FILE ASSIGN
     PRT.FIL.
     SELECT INPUT-FILE ASSIGN INP.FIL.
 DATA DIVISION.
 FILE SECTION.
 FD  PRINT-FILE
     LABEL RECORDS
     OMITTED.
 01 OUTPUT-LINE PIC X(80).

 FD INPUT-FILE
    LABEL RECORDS OMITTED.
 01 INPUT-LINE.
   02 SEQ-NUMBER PIC X(6).
   02 INFO       PIC X(74).

 WORKING-STORAGE SECTION.
 01 A PIC 99.
 01 B PIC 99.

 PROCEDURE DIVISION.
 PARA-A.
    ACCEPT A.
    ACCEPT B.
    IF A EQUAL B
      PERFORM PARA-B
    ELSE 
      PERFORM PARA-C END-IF.
    PERFORM PARA-B.
    STOP RUN.

 PARA-B.
    DISPLAY A.

 PARA-C.
    DISPLAY B.
