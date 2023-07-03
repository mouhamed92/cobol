       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERG-STD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


            SELECT  STDFILE ASSIGN TO
             "C:/work space/Cobol path/labs/Cobol/challenge/ACME.DAT"
            FILE STATUS IS  STD-KEY-CHECK
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT STDNEWFILE ASSIGN TO
           "C:/work space/Cobol path/labs/Cobol/challenge/FUSESINC.DAT"
             FILE STATUS IS  STDN-KEY-CHECK
             ORGANIZATION IS LINE SEQUENTIAL.

            SELECT MERGEDFILE ASSIGN TO
           "C:/work space/Cobol path/labs/Cobol/challenge/FINAL.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.

             SELECT REP-FILE ASSIGN TO
           "C:/work space/Cobol path/labs/Cobol/challenge/REPORT.DAT"
             ORGANIZATION IS LINE SEQUENTIAL.

             SELECT WORK-FILE ASSIGN TO "WORK.TEMP".

       DATA DIVISION.
       FILE SECTION.

           FD STDFILE.
           01 ACMEDETAILS     PIC X(47).

           FD STDNEWFILE.
           01 FUSESDETAILS  PIC X(47).

           FD MERGEDFILE.
           01 SORTDETAILS.
           88 SORTEOF            VALUE HIGH-VALUES.
             02 SF-SSN         PIC 9(9).
             02 SF-LASTNAME   PIC X(10).
             02 SF-FIRSTNAME  PIC X(10).
             02 FILLER        PIC X(18).

           SD WORK-FILE.
           01 WORK-DETAILS.
             02 SSN     PIC 9(9).
             02 FILLER  PIC X(38).

           FD REP-FILE.
           01 REP-DETAIL PIC X(132).

       WORKING-STORAGE SECTION.

       01 STDN-KEY-CHECK  PIC X(2).
       01 STD-KEY-CHECK  PIC X(2).

       01 REPORT-DATA.
           02 WS-SSN         PIC 9(9).
           02 FILLER        PIC X(5).
           02 WS-LASTNAME   PIC X(10).
           02 FILLER        PIC X(5).
           02 WS-FIRSTNAME  PIC X(10).
           02 FILLER        PIC X(73).

       PROCEDURE DIVISION.

            OPEN INPUT STDFILE,STDNEWFILE

                 IF STD-KEY-CHECK NOT = "00"
                     DISPLAY "ERROR, SATATUS CODE :"STD-KEY-CHECK
                     GO TO 900-END-PROGRAM
                 END-IF.

                 IF STDN-KEY-CHECK NOT = "00"
                     DISPLAY "ERROR, SATATUS CODE :"STDN-KEY-CHECK
                     GO TO 900-END-PROGRAM
                 END-IF.

                 MERGE WORK-FILE ON ASCENDING KEY SSN
                 USING STDFILE,STDNEWFILE
                 GIVING MERGEDFILE.

                 OPEN INPUT MERGEDFILE
                 OPEN OUTPUT REP-FILE

                  READ MERGEDFILE
                  AT END SET SORTEOF TO TRUE
                  END-READ.

                  PERFORM 100-PROCESS-DATA UNTIL SORTEOF.
                  PERFORM 900-END-PROGRAM.

           100-PROCESS-DATA.
                 MOVE SF-SSN TO WS-SSN.
                 MOVE SF-LASTNAME TO WS-LASTNAME.
                 MOVE SF-FIRSTNAME TO WS-FIRSTNAME.
                 WRITE REP-DETAIL FROM REPORT-DATA AFTER
                 ADVANCING 1 LINE.

               READ MERGEDFILE
                 AT END SET SORTEOF TO TRUE
               END-READ.

           900-END-PROGRAM.
           CLOSE STDFILE,STDNEWFILE,MERGEDFILE,REP-FILE

            STOP RUN.

       END PROGRAM MERG-STD.
