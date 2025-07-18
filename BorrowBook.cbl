      ******************************************************************
      * Author:Kaung Khant Nyein
      * Date: 11.7.2025
      * Purpose: Borrow a book form library
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BorrowBook.

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
       01  id_to_email        PIC X(70).
       01  gender_n_flag      PIC X(11).
       01  dummy PIC X.
       01  comma_in_addr PIC 9.
       01  FOUND-MEMBER       PIC X VALUE 'N'.
       01  FOUND-BOOK         PIC X VALUE 'N'.
       01  VALID-FLAG         PIC X VALUE 'N'.
       01  FILE-END           PIC X VALUE 'N'.
       01  MAX-TRAN-ID        PIC 9(5) VALUE 0.
       01  CURR-TRAN-ID       PIC 9(5).
       01  NEW-TRAN-ID        PIC 9(5).
       01  CURR-TRAN-ID-X     PIC X(5).
       01  DUE-FLAG           PIC X(3) VALUE "NO".
       01  TODAY              PIC 9(8).
       01  WS-DAY             PIC 9(2).
       01  WS-MONTH           PIC 9(2).
       01  WS-YEAR            PIC 9(4).
       01  START-DATE         PIC 9(10).
       01  RES                PIC 9(8).
       01  END-DATE           PIC 9(10).
       01  END-DAY            PIC 9(2).
       01  END-MONTH          PIC 9(2).
       01  END-YEAR           PIC 9(4).
       01  WS-DAYS-TO-ADD     PIC 9(3) VALUE 14.


       01  BOOK-TABLE.
           05 BOOK-ENTRY OCCURS 100 TIMES.
              10  BK-ID     PIC X(5).
              10  BK-NAME   PIC X(30).
              10  BK-AUTHOR PIC X(30).
              10  BK-COUNT  PIC 99.
              10  BK-GENRE  PIC X(30).
       01  BK-IDX         PIC 9(3) VALUE 1.
       01  BK-TOTAL       PIC 9(3) VALUE 0.
       01  BK-MATCH-IDX   PIC 9(3) VALUE 0.

       01  FOUND-ID-FLAG     PIC X VALUE 'N'.  *> Book ID found flag
       01  BOOK-AVAILABLE    PIC X VALUE 'N'.  *> Book count > 0 flag

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.
       MAIN-PROCEDURE.

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

                   INSPECT MEMBER-REC TALLYING comma_in_addr FOR ALL '"'
                   IF comma_in_addr >0 THEN
                   UNSTRING MEMBER-REC DELIMITED BY '"'
                   INTO id_to_email, MEMBER-ADDR, gender_n_flag
                   UNSTRING id_to_email DELIMITED BY ','
                   INTO MEMBER-ID-F, MEMBER-NAME, MEMBER-EMAIL
                   UNSTRING gender_n_flag DELIMITED BY ','
                   INTO dummy, MEMBER-GENDER, MEMBER-FLAG

                   ELSE
                   UNSTRING MEMBER-REC DELIMITED BY ","
                       INTO MEMBER-ID-F, MEMBER-NAME, MEMBER-EMAIL,
                            MEMBER-ADDR, MEMBER-GENDER, MEMBER-FLAG
                   END-IF

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


       MOVE 1 TO BK-IDX
       MOVE 0 TO BK-TOTAL
       MOVE 'N' TO FILE-END

       OPEN INPUT BOOK-FILE
       PERFORM UNTIL FILE-END = 'Y'
       READ BOOK-FILE
        AT END
            MOVE 'Y' TO FILE-END
        NOT AT END
            UNSTRING BOOK-REC DELIMITED BY ","
                INTO BK-ID(BK-IDX), BK-NAME(BK-IDX),
                     BK-AUTHOR(BK-IDX), BK-COUNT(BK-IDX),
                     BK-GENRE(BK-IDX)

            IF BK-ID(BK-IDX) = WS-BOOK-ID
                MOVE 'Y' TO FOUND-ID-FLAG

                IF BK-COUNT(BK-IDX) > 0
                    COMPUTE BK-COUNT(BK-IDX) = BK-COUNT(BK-IDX) - 1
                    MOVE BK-IDX TO BK-MATCH-IDX
                    MOVE 'Y' TO BOOK-AVAILABLE
                END-IF
            END-IF

            ADD 1 TO BK-IDX
            ADD 1 TO BK-TOTAL
       END-PERFORM

       CLOSE BOOK-FILE

       IF FOUND-ID-FLAG = 'N'
           DISPLAY "Book not found."
       STOP RUN
       ELSE
           IF BOOK-AVAILABLE = 'N'
               DISPLAY "Book is currently out of stock."
        STOP RUN
           END-IF
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


       MOVE FUNCTION CURRENT-DATE(1:8) TO TODAY
       MOVE TODAY(1:4) TO WS-YEAR
       MOVE TODAY(5:2) TO WS-MONTH
       MOVE TODAY(7:2) TO WS-DAY
       STRING WS-DAY DELIMITED BY SIZE "-"
              WS-MONTH   DELIMITED BY SIZE "-"
              WS-YEAR  DELIMITED BY SIZE
           INTO START-DATE

       COMPUTE RES = FUNCTION INTEGER-OF-DATE (TODAY)
       ADD WS-DAYS-TO-ADD TO RES
       COMPUTE TODAY = FUNCTION DATE-OF-INTEGER (RES)
       MOVE TODAY(1:4) TO END-YEAR
       MOVE TODAY(5:2) TO END-MONTH
       MOVE TODAY(7:2) TO END-DAY
       STRING END-DAY DELIMITED BY SIZE "-"
              END-MONTH   DELIMITED BY SIZE "-"
              END-YEAR  DELIMITED BY SIZE
           INTO END-DATE

       IF MAX-TRAN-ID = 0 THEN
           OPEN OUTPUT LOG-FILE
           MOVE 30001 TO NEW-TRAN-ID
       ELSE
           OPEN EXTEND LOG-FILE
       END-IF
      *>  OPEN EXTEND LOG-FILE
       STRING NEW-TRAN-ID DELIMITED BY SIZE ","
              WS-MEMBER-ID DELIMITED BY SIZE ","
              WS-BOOK-ID DELIMITED BY SIZE ","
              START-DATE DELIMITED BY SIZE ","
              END-DATE DELIMITED BY SIZE ","
              DUE-FLAG DELIMITED BY SIZE ","
              SPACE DELIMITED BY SIZE
           INTO LOG-REC
       WRITE LOG-REC
       CLOSE LOG-FILE


       OPEN OUTPUT BOOK-FILE
       PERFORM VARYING BK-IDX FROM 1 BY 1 UNTIL BK-IDX > BK-TOTAL
           STRING BK-ID(BK-IDX) DELIMITED BY SIZE ","
                  BK-NAME(BK-IDX) DELIMITED BY SIZE ","
                  BK-AUTHOR(BK-IDX) DELIMITED BY SIZE ","
                  BK-COUNT(BK-IDX) DELIMITED BY SIZE ","
                  BK-GENRE(BK-IDX) DELIMITED BY SIZE
               INTO BOOK-REC
           WRITE BOOK-REC
       END-PERFORM
       CLOSE BOOK-FILE

       DISPLAY "Book borrowed successfully. Transaction ID: "
       NEW-TRAN-ID.
       END PROGRAM BorrowBook.
