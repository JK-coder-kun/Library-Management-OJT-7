       IDENTIFICATION DIVISION.
       PROGRAM-ID. DueDateCheck.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-BORROW-DATE         PIC 9(8).
       01 WS-DUE-DATE            PIC 9(8).
       01 WS-RETURN-DATE         PIC 9(8).


       01 WS-BORROW-YEAR         PIC 9(4).
       01 WS-BORROW-MONTH        PIC 9(2).
       01 WS-BORROW-DAY          PIC 9(2).

       01 WS-DUE-YEAR            PIC 9(4).
       01 WS-DUE-MONTH           PIC 9(2).
       01 WS-DUE-DAY             PIC 9(2).

       01 WS-RETURN-YEAR         PIC 9(4).
       01 WS-RETURN-MONTH        PIC 9(2).
       01 WS-RETURN-DAY          PIC 9(2).

       01 WS-FORMATTED-DUE-DATE.
           05 FILLER             PIC 9(4).
           05 FILLER             PIC 9(2).
           05 FILLER             PIC 9(2).

       01 WS-DAYS-TO-ADD         PIC 9(3) VALUE 30.

       01 res     PIC 9(8).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-BORROW-DATE

           COMPUTE res = FUNCTION INTEGER-OF-DATE (WS-BORROW-DATE)
           ADD WS-DAYS-TO-ADD TO res
           COMPUTE WS-DUE-DATE = FUNCTION DATE-OF-INTEGER (res)

           DISPLAY WS-DUE-DATE
           DISPLAY "Enter return date (YYYYMMDD): "
           ACCEPT WS-RETURN-DATE

           IF WS-RETURN-DATE > WS-DUE-DATE
               DISPLAY "DUE, fine"
           ELSE
               DISPLAY "NAh"
           END-IF

           STOP RUN.
