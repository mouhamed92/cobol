        IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTINGSTUDENTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT STUDENTSFILE ASSIGN TO
            "C:/work space/Cobol path/labs/Cobol/STUDENTS.DAT"
             FILE STATUS  IS KEY-STD-CHECK
             ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORTEDFILE ASSIGN TO
              "C:/work space/Cobol path/labs/Cobol/NEWSTUDENTS.DAT"
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT WORKFILE ASSIGN TO "WORK.TEMP".

       DATA DIVISION.
       FILE SECTION.

       FD STUDENTSFILE.
       01 STUDENTDETAILS     PIC X(40).

       FD SORTEDFILE.
       01 STUDENTDETAILS     PIC X(40).

       SD WORKFILE.
       01 WORKDETAILS.
          02 FILLER        PIC 9(7).
          02 WSTUDENTLNAME PIC X(10).
          02 WSTUDENTFNAME PIC X(10).
          02 FILLER        PIC X(9).
          02 WMAJOR        PIC X(3).
          02 FILLER        PIC X.

       WORKING-STORAGE SECTION.

       01 KEY-STD-CHECK  PIC X(2).

       PROCEDURE DIVISION.
       0100-SORT-STUDENT.

           OPEN INPUT STUDENTSFILE

           IF KEY-STD-CHECK NOT = "00"
               DISPLAY "ERROR CODE STATUS: ", KEY-STD-CHECK
               GO TO 9000-END-PROGRAM
           END-IF.

           SORT WORKFILE ON ASCENDING KEY WMAJOR
             USING STUDENTSFILE
             GIVING SORTEDFILE.
            PERFORM 9000-END-PROGRAM.

       0100-END.

       9000-END-PROGRAM.
           CLOSE STUDENTSFILE.
           STOP RUN.

          END PROGRAM SORTINGSTUDENTS.
