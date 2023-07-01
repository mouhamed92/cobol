       IDENTIFICATION DIVISION.
       PROGRAM-ID. empRaise.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT EMPFILE ASSIGN TO
               "C:/work space/Cobol path/labs/Cobol/EMPFILE.DAT"
               FILE STATUS IS KEY-EMP-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.

             SELECT NEWEMPFILE ASSIGN TO
               "C:/work space/Cobol path/labs/Cobol/NEWEMPFILE"
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

       FD NEWEMPFILE.
       01 NEWEMPLOYEE.
         05 NEWEMPDATA  PIC  X(38).
         05 NEWHOURLYRATE          PIC 9(4)V99.
         05 NEWDEPARTMENT          PIC X(30).
         05 NEWGENDER              PIC X.

       WORKING-STORAGE SECTION.

       01 WS-EMP-COUNT PIC 9(3).
       01 KEY-EMP-STATUS PIC  X(2).

       PROCEDURE DIVISION.

           100-READ-FILE.
           OPEN INPUT EMPFILE
           OPEN OUTPUT NEWEMPFILE
           INITIALIZE WS-EMP-COUNT

            IF KEY-EMP-STATUS NOT = "00"
               DISPLAY "ERROR AT PRECESSING, CODE STATUS: ",
               KEY-EMP-STATUS
               GO TO 9000-END-PROGRAM
            END-IF.

            READ EMPFILE
             AT END SET ENDOFFILE TO TRUE
            END-READ.

             PERFORM 200-PROCESS-FILE THRU 200-END UNTIL ENDOFFILE.
             PERFORM 9000-END-PROGRAM.
           100-END.


           200-PROCESS-FILE.
               MOVE EMPINFO TO NEWEMPDATA.
               COMPUTE NEWHOURLYRATE = HOURLYRATE * 1.03.
               MOVE DEPARTMENT TO NEWDEPARTMENT.
               MOVE GENDER TO NEWGENDER.
               ADD 1 TO WS-EMP-COUNT.
               WRITE NEWEMPLOYEE.

               READ EMPFILE
                AT END SET ENDOFFILE TO TRUE
               END-READ.

           200-END.

           9000-END-PROGRAM.
             CLOSE EMPFILE,NEWEMPFILE.
             DISPLAY "NUMBER OF PROCESSED EMPLOYEES: " WS-EMP-COUNT

       STOP RUN.
      ** add other procedures here
       END PROGRAM empRaise.
