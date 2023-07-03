       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-EMP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT EMP-PAYROLL ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/NEWEMPFILE"
            FILE STATUS IS  EMP-KEY-CHECK
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORTED-EM ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/SORTED-EMP"
             ORGANIZATION IS LINE SEQUENTIAL.

             SELECT WORK-TEMP ASSIGN TO "WORK.TEMP".

       DATA DIVISION.
       FILE SECTION.

           FD EMP-PAYROLL.
           01 EMP-DETAILS.
               05 EMP-DATA PIC X(61).

           FD SORTED-EM.
           01 SOR-DETAILS.
               05 SOR-DTAA PIC X(61).

           SD WORK-TEMP.
           01 WORK-DATA.
             05 PAY-EMPID    PIC 9(7).
             05 PAY-EMP-FN   PIC X(10).
             05 PAY-EMP-LN   PIC X(10).
             05 PAY-AMOUNT    PIC 9(4)V99.
             05 PAY-DEP      PIC X(30).

       WORKING-STORAGE SECTION.

       01 EMP-KEY-CHECK  PIC X(2).

       PROCEDURE DIVISION.

           OPEN INPUT EMP-PAYROLL
            IF EMP-KEY-CHECK NOT = "00"
                DISPLAY "ERROR, CODE STATUS :" EMP-KEY-CHECK
                GO TO 900-END-PROGRAM
            END-IF.

            SORT WORK-TEMP ON DESCENDING KEY PAY-AMOUNT
             USING EMP-PAYROLL
             GIVING SORTED-EM
             PERFORM 900-END-PROGRAM.

           900-END-PROGRAM.
            CLOSE EMP-PAYROLL.

            STOP RUN.

       END PROGRAM SORT-EMP.
