       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-PAYROLL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EMPFILE ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/EMPFILE.DAT"
              FILE STATUS IS KEY-EMP-STATUS
              ORGANIZATION IS LINE SEQUENTIAL.

            SELECT PAYROLL ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/PAYROLL"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPFILE.
       01 EMPDETAILS.
         88 ENDOFFILE VALUE HIGH-VALUE.
         05 EMPDATA             PIC X(38).
         05 EMPINFO  REDEFINES EMPDATA.
           10 EMP-ID            PIC 9(7).
           10 EMPLOYEENAME.
              15 LASTNAME       PIC X(10).
              15 FIRSTNAME      PIC X(10).
           10 STARTDATE.
              15 START-YEAR     PIC 9(4).
              15 START-MONTH    PIC 9(2).
              15 START-DAY      PIC 9(2).
           10 HOURSWORKED       PIC 9(3).
         05 HOURLYRATE          PIC 9(4)V99.
         05 DEPARTMENT          PIC X(30).
         05 GENDER              PIC X.

       FD PAYROLL.
       01 PAY-EMP-DETAILS.
         05 PAY-EMPID          PIC 9(7).
         05 PAY-EMPFNAME       PIC X(10).
         05 PAY-EMPLNAME       PIC X(10).
         05 PAY-AMOUNT         PIC 9(4)V99.
         05 PAY-DEPARTMENT     PIC X(30).

       WORKING-STORAGE SECTION.

       01 KEY-EMP-STATUS PIC X(2).
       01 WS-COUNT-EMP   PIC 9(3).

       PROCEDURE DIVISION.

           100-READ-EMP.
            OPEN INPUT EMPFILE
            OPEN OUTPUT PAYROLL
            INITIALIZE WS-COUNT-EMP
            IF KEY-EMP-STATUS NOT = "00"
                DISPLAY "ERROR FILE PROCESSING, CODE STATUS: ",
                 KEY-EMP-STATUS
                 GO TO 9000-END-PROGRAM
            END-IF.

              READ EMPFILE
              AT END SET ENDOFFILE TO TRUE
              END-READ.

              PERFORM 200-PROCESS-EMP UNTIL ENDOFFILE.
              PERFORM 9000-END-PROGRAM.

           100-END.

           200-PROCESS-EMP.
             MOVE EMP-ID TO PAY-EMPID.
             MOVE FIRSTNAME TO PAY-EMPFNAME.
             MOVE LASTNAME  TO PAY-EMPLNAME
             COMPUTE PAY-AMOUNT = HOURLYRATE * HOURSWORKED.
             MOVE DEPARTMENT TO PAY-DEPARTMENT.
             ADD 1 TO WS-COUNT-EMP.
             WRITE PAY-EMP-DETAILS.

             READ EMPFILE
             AT END SET ENDOFFILE TO TRUE
             END-READ.
           200-END.

           9000-END-PROGRAM.
             CLOSE EMPFILE,PAYROLL
              DISPLAY "EMPLOYEE PROCESSED NUMBER: "WS-COUNT-EMP

            STOP RUN.
      ** add other procedures here
       END PROGRAM EMP-PAYROLL.
