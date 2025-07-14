       IDENTIFICATION DIVISION.
       PROGRAM-ID. BORROWBOOK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEMBER-FILE ASSIGN TO "../members.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BOOK-FILE ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOG-FILE ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MEMBER-FILE.
       01  MEMBER-REC         PIC X(200).
       FD  BOOK-FILE.
       01  BOOK-REC           PIC X(200).
       FD  LOG-FILE.
       01  LOG-REC            PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-MEMBER-ID       PIC X(5).
       01  WS-BOOK-ID         PIC X(5).
       01  MEMBER-ID-F        PIC X(5).
       01  MEMBER-NAME        PIC X(30).
       01  MEMBER-EMAIL       PIC X(35).
       01  MEMBER-ADDR        PIC X(50).
       01  MEMBER-GENDER      PIC X.
       01  MEMBER-FLAG        PIC X(8).
       01  BOOK-ID-F          PIC X(5).
       01  BOOK-NAME          PIC X(30).
       01  BOOK-AUTHOR        PIC X(30).
       01  BOOK-COUNT         PIC 99.
       01  BOOK-GENRE         PIC X(30).
       01  FOUND-MEMBER       PIC X VALUE 'N'.
       01  FOUND-BOOK         PIC X VALUE 'N'.
       01  VALID-FLAG         PIC X VALUE 'N'.
       01  FILE-END           PIC X VALUE 'N'.
       01  MAX-TRAN-ID        PIC 9(5) VALUE 0.
       01  CURR-TRAN-ID       PIC 9(5).
       01  NEW-TRAN-ID        PIC 9(5).
       01  CURR-TRAN-ID-X     PIC X(5).
       01  TODAY              PIC 9(8).
       01  WS-DAY             PIC 9(2).
       01  WS-MONTH            PIC 9(2).
       01  WS-YEAR            PIC 9(2).
       01  START-DATE         PIC 9(8).
       01  res                PIC 9(8).
       01  END-DAY             PIC 9(2).
       01  END-MONTH            PIC 9(2).
       01  END-YEAR            PIC 9(2).
       01  END-DATE         PIC 9(8).
       01 WS-DAYS-TO-ADD         PIC 9(3) VALUE 30.

       PROCEDURE DIVISION.

       DISPLAY "Enter Member ID: "
       ACCEPT WS-MEMBER-ID
       DISPLAY "Enter Book ID: "
       ACCEPT WS-BOOK-ID

       OPEN INPUT MEMBER-FILE
       PERFORM UNTIL FILE-END = 'Y'
           READ MEMBER-FILE
               AT END
                   MOVE 'Y' TO FILE-END
               NOT AT END
                   UNSTRING MEMBER-REC DELIMITED BY ","
                       INTO MEMBER-ID-F, MEMBER-NAME, MEMBER-EMAIL,
                       MEMBER-ADDR,
                            MEMBER-GENDER, MEMBER-FLAG
                   IF MEMBER-ID-F = WS-MEMBER-ID
                       IF MEMBER-FLAG = "ACTIVE"
                           MOVE 'Y' TO VALID-FLAG
                       END-IF
                       MOVE 'Y' TO FOUND-MEMBER
           END-READ
       END-PERFORM
       CLOSE MEMBER-FILE

       IF VALID-FLAG NOT = 'Y'
           DISPLAY "Invalid member or inactive status."
           STOP RUN
       END-IF

       MOVE 'N' TO FILE-END
       OPEN INPUT BOOK-FILE
       PERFORM UNTIL FILE-END = 'Y'
           READ BOOK-FILE
               AT END
                   MOVE 'Y' TO FILE-END
               NOT AT END
                   UNSTRING BOOK-REC DELIMITED BY ","
                       INTO BOOK-ID-F, BOOK-NAME, BOOK-AUTHOR,
                       BOOK-COUNT, BOOK-GENRE
                   IF BOOK-ID-F = WS-BOOK-ID AND BOOK-COUNT > 0
                       COMPUTE BOOK-COUNT = BOOK-COUNT - 1
                       STRING BOOK-ID-F DELIMITED BY SIZE ","
                              BOOK-NAME DELIMITED BY SIZE ","
                              BOOK-AUTHOR DELIMITED BY SIZE ","
                              BOOK-COUNT DELIMITED BY SIZE ","
                              BOOK-GENRE DELIMITED BY SIZE
                              INTO BOOK-REC
                       MOVE 'Y' TO FOUND-BOOK
           END-READ
       END-PERFORM
       CLOSE BOOK-FILE

       IF FOUND-BOOK NOT = 'Y'
           DISPLAY "Book not found or unavailable."
           STOP RUN
       END-IF

       MOVE 'N' TO FILE-END
       OPEN INPUT LOG-FILE
       PERFORM UNTIL FILE-END = 'Y'
           READ LOG-FILE
               AT END
                   MOVE 'Y' TO FILE-END
               NOT AT END
                   UNSTRING LOG-REC DELIMITED BY "," INTO CURR-TRAN-ID-X
                   MOVE CURR-TRAN-ID-X TO CURR-TRAN-ID
                   IF CURR-TRAN-ID > MAX-TRAN-ID
                       MOVE CURR-TRAN-ID TO MAX-TRAN-ID
           END-READ
       END-PERFORM
       CLOSE LOG-FILE

       COMPUTE NEW-TRAN-ID = MAX-TRAN-ID + 1

       MOVE FUNCTION CURRENT-DATE(1:8) TO Today
       MOVE Today(1:4) TO WS-YEAR
       MOVE Today(5:2) TO WS-MONTH
       MOVE Today(7:2) TO WS-DAY
       STRING WS-DAY DELIMITED BY SIZE "/"
              WS-MONTH DELIMITED BY SIZE "/"
              WS-YEAR DELIMITED BY SIZE
           INTO START-DATE

       COMPUTE res = FUNCTION INTEGER-OF-DATE (Today)
       ADD WS-DAYS-TO-ADD TO res
       COMPUTE END-DATE = FUNCTION DATE-OF-INTEGER (res)

       MOVE END-DATE(1:4) TO END-YEAR
       MOVE END-DATE(5:2) TO END-MONTH
       MOVE END-DATE(7:2) TO END-DAY
       STRING END-DAY DELIMITED BY SIZE "/"
              END-MONTH DELIMITED BY SIZE "/"
              END-YEAR DELIMITED BY SIZE
           INTO END-DATE

       OPEN EXTEND LOG-FILE
       STRING NEW-TRAN-ID DELIMITED BY SIZE ","
              WS-MEMBER-ID DELIMITED BY SIZE ","
              WS-BOOK-ID DELIMITED BY SIZE ","
              START-DATE DELIMITED BY SIZE ","
              END-DATE DELIMITED BY SIZE ","
              "OFF" DELIMITED BY SIZE ","
              Space DELIMITED BY SIZE
           INTO LOG-REC
       WRITE LOG-REC
       CLOSE LOG-FILE

       OPEN OUTPUT BOOK-FILE
       WRITE BOOK-REC
       CLOSE BOOK-FILE

       DISPLAY "Book borrowed successfully. Transaction ID: "
       NEW-TRAN-ID
       STOP RUN.
