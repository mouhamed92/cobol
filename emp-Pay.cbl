       IDENTIFICATION DIVISION.
       PROGRAM-ID. emp-Pay.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT EMP-PAYROLL ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/EMPFILE.DAT"
            FILE STATUS IS  EMP-KEY-CHECK
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT EMP-FILE-I ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/NEWEMPFILE"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMP-PAYROLL.
       01 EMPDETAILS.
           88 EOF   VALUES HIGH-VALUES.
           05 EMPDATA  PIC  X(38).
           05 EMPINFO REDEFINES EMPDATA.
             10 EMPID  PIC 9(7).
             10 EMPNAME.
               15 EMPFNAME  PIC X(10).
               15 EMPLNAME  PIC X(10).
             10 STARTDATE.
               15 STARTMHT  PIC 9(2).
               15 STARTDAY  PIC 9(2).
               15 STARTYER  PIC 9(4).
             10 HOURLYWRK   PIC 9(3).
           05 HOURLYRATE    PIC 9(4)V99.
           05 DEPARTMENT    PIC X(30).
           05 GENDER        PIC X.

       FD EMP-FILE-I.
       01 EMP-DET-PAY.
         05 PAY-EMPID    PIC 9(7).
         05 PAY-EMP-FN   PIC X(10).
         05 PAY-EMP-LN   PIC X(10).
         05 PAY-AMOUNT    PIC 9(4)V99.
         05 PAY-DEP      PIC X(30).

       WORKING-STORAGE SECTION.

       01 EMP-KEY-CHECK PIC X(2).
       01 COUNT-EMP     PIC 9(3).

       PROCEDURE DIVISION.

            OPEN INPUT EMP-PAYROLL
            OPEN OUTPUT EMP-FILE-I

            IF EMP-KEY-CHECK  NOT = "00"
                DISPLAY "ERROR, FILE STATUS : " EMP-KEY-CHECK
                GO TO 900-END-PROGRAM
            END-IF.

            READ EMP-PAYROLL
             AT END SET EOF TO TRUE
            END-READ.

             PERFORM 100-PROCESS-FILE UNTIL EOF.
             PERFORM 900-END-PROGRAM.

           100-PROCESS-FILE.

              MOVE EMPID TO PAY-EMPID.
              MOVE EMPFNAME TO PAY-EMP-FN.
              MOVE EMPLNAME TO PAY-EMP-LN.
              COMPUTE PAY-AMOUNT = HOURLYWRK * HOURLYRATE.
              MOVE DEPARTMENT TO PAY-DEP.
              WRITE EMP-DET-PAY.

              ADD 1 TO COUNT-EMP.

              READ EMP-PAYROLL
              AT END SET EOF TO TRUE
              END-READ.

           100-END.

           900-END-PROGRAM.
           CLOSE EMP-PAYROLL, EMP-FILE-I.
            DISPLAY "EMPLOYEE PROCESSED NUMBER: "COUNT-EMP.
           900-END.

            STOP RUN.

       END PROGRAM emp-Pay.
