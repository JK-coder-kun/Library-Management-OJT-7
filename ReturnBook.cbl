      ******************************************************************
      * Author: Htay Lwin & Kaung Khant Nyein
      * Date: 15.7.2025
      * Purpose: Return Book with fine handling and proper CSV updates
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReturnBook.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BOOK-FILE ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FINE-FILE ASSIGN TO "../fine.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-REC         PIC X(200).
       FD  BOOK-FILE.
       01  BOOK-REC        PIC X(200).
       FD  FINE-FILE.
       01  FINE-REC        PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-MEMBER-ID     PIC X(5).
       01  WS-BOOK-ID       PIC X(5).
       01  TRAN-ID-F        PIC X(5).
       01  MEMBER-ID-F      PIC X(5).
       01  BOOK-ID-F        PIC X(5).
       01  START-DATE       PIC X(10).
       01  END-DATE         PIC X(10).
       01  DUE-FLAG         PIC X(3).
       01  RETURN-DATE-F    PIC X(10).
       01  RETURN-DATE-FF    PIC X(10).
       01  FILE-END         PIC X VALUE "N".
       01  FOUND            PIC X VALUE "N".
       01  TODAY-INT        PIC 9(8).
       01  INT-RETURN       PIC 9(8).
       01  INT-END          PIC 9(8).
       01  DIFF-DAYS        PIC 9(3).
       01  FINE-AMOUNT      PIC 9(5).
       01  CONFIRM          PIC X.
       01  MAX-FINE-ID      PIC 9(5) VALUE 0.
       01  CURR-FINE-ID-X   PIC X(5).
       01  CURR-FINE-ID     PIC 9(5).
       01  NEW-FINE-ID      PIC 9(5).
       01  WS-NEW-LOG-ENTRIES.
           05 LOG-LINE OCCURS 100 TIMES.
               10 LOG-DATA    PIC X(200).
       01  LOG-IDX          PIC 9(3) VALUE 0.
       01  BOOK-TABLE.
           05 BOOK-ENTRY OCCURS 100 TIMES.
               10  BK-ID     PIC X(5).
               10  BK-NAME   PIC X(30).
               10  BK-AUTHOR PIC X(30).
               10  BK-COUNT  PIC 99.
               10  BK-GENRE  PIC X(30).
       01  BK-IDX       PIC 9(3).
       01  BK-TOTAL     PIC 9(3) VALUE 0.
       01  WS-END-YYYYMMDD    PIC 9(8).
       01  WS-END-YYYY        PIC X(4).
       01  WS-END-MM          PIC X(2).
       01  WS-END-DD          PIC X(2).
       01  WS-RETURN-YYYYMMDD PIC 9(8).
       01  WS-RETURN-DD       PIC X(2).
       01  WS-RETURN-MM       PIC X(2).
       01  WS-RETURN-YYYY     PIC X(4).

       PROCEDURE DIVISION.

       DISPLAY "Enter Member ID: "
       ACCEPT WS-MEMBER-ID
       DISPLAY "Enter Book ID: "
       ACCEPT WS-BOOK-ID

       MOVE FUNCTION CURRENT-DATE(1:8) TO TODAY-INT
       MOVE TODAY-INT(1:4) TO WS-RETURN-YYYY
       MOVE TODAY-INT(5:2) TO WS-RETURN-MM
       MOVE TODAY-INT(7:2) TO WS-RETURN-DD
       STRING WS-RETURN-DD DELIMITED BY SIZE "-"
              WS-RETURN-MM DELIMITED BY SIZE "-"
              WS-RETURN-YYYY DELIMITED BY SIZE
              INTO RETURN-DATE-F
       STRING WS-RETURN-YYYY DELIMITED BY SIZE
              WS-RETURN-MM   DELIMITED BY SIZE
              WS-RETURN-DD   DELIMITED BY SIZE
              INTO WS-RETURN-YYYYMMDD

       OPEN INPUT LOG-FILE
       MOVE 0 TO LOG-IDX
       MOVE "N" TO FILE-END
       PERFORM UNTIL FILE-END = "Y"
           READ LOG-FILE
               AT END
                   MOVE "Y" TO FILE-END
               NOT AT END
                   ADD 1 TO LOG-IDX
                   MOVE LOG-REC TO LOG-DATA(LOG-IDX)

                   UNSTRING LOG-REC DELIMITED BY ","
                       INTO TRAN-ID-F, MEMBER-ID-F, BOOK-ID-F,
                     START-DATE, END-DATE, DUE-FLAG, RETURN-DATE-FF

                   IF MEMBER-ID-F = WS-MEMBER-ID AND
                      BOOK-ID-F = WS-BOOK-ID AND
                      RETURN-DATE-FF = SPACE
                       MOVE "Y" TO FOUND

                       IF DUE-FLAG = "NO"
                           DISPLAY "Book returned on time."
                       ELSE
                           MOVE END-DATE(7:4) TO WS-END-YYYY
                           MOVE END-DATE(1:2) TO WS-END-DD
                           MOVE END-DATE(4:2) TO WS-END-MM
                           STRING WS-END-YYYY DELIMITED BY SIZE
                                  WS-END-MM   DELIMITED BY SIZE
                                  WS-END-DD   DELIMITED BY SIZE
                                  INTO WS-END-YYYYMMDD
            MOVE FUNCTION INTEGER-OF-DATE(WS-END-YYYYMMDD) TO INT-END
           MOVE FUNCTION INTEGER-OF-DATE(WS-RETURN-YYYYMMDD)
           TO INT-RETURN
                           COMPUTE DIFF-DAYS = INT-RETURN - INT-END
                           COMPUTE FINE-AMOUNT = DIFF-DAYS * 1000
                        DISPLAY "Overdue by ", DIFF-DAYS, " days."
                        DISPLAY "Fine amount: ", FINE-AMOUNT, " MMK"
                           DISPLAY "Return and confirm payment (Y/N)?"
                           ACCEPT CONFIRM
                           IF CONFIRM NOT = "Y"
                               DISPLAY "Return canceled."
                               STOP RUN
                           END-IF
                           OPEN INPUT FINE-FILE
                           MOVE "N" TO FILE-END
                           PERFORM UNTIL FILE-END = "Y"
                               READ FINE-FILE
                                   AT END
                                       MOVE "Y" TO FILE-END
                                   NOT AT END
                  UNSTRING FINE-REC DELIMITED BY "," INTO CURR-FINE-ID-X
                                    MOVE CURR-FINE-ID-X TO CURR-FINE-ID
                                       IF CURR-FINE-ID > MAX-FINE-ID
                                      MOVE CURR-FINE-ID TO MAX-FINE-ID
                               END-READ
                           END-PERFORM
                           CLOSE FINE-FILE
                           COMPUTE NEW-FINE-ID = MAX-FINE-ID + 1
      *>                      OPEN EXTEND FINE-FILE
                           IF MAX-FINE-ID = 0 THEN
                               OPEN OUTPUT FINE-FILE
                               MOVE 00001 TO NEW-FINE-ID
                           ELSE
                               OPEN EXTEND FINE-FILE
                           END-IF

                           STRING NEW-FINE-ID DELIMITED BY SIZE ","
                                  TRAN-ID-F DELIMITED BY SIZE ","
                                  MEMBER-ID-F DELIMITED BY SIZE ","
                                  DIFF-DAYS DELIMITED BY SIZE ","
                                  FINE-AMOUNT DELIMITED BY SIZE
                                  INTO FINE-REC
                           WRITE FINE-REC
                           CLOSE FINE-FILE
                       END-IF

                       STRING TRAN-ID-F DELIMITED BY SIZE ","
                              MEMBER-ID-F DELIMITED BY SIZE ","
                              BOOK-ID-F DELIMITED BY SIZE ","
                              START-DATE DELIMITED BY SIZE ","
                              END-DATE DELIMITED BY SIZE ","
                              DUE-FLAG DELIMITED BY SIZE ","
                              RETURN-DATE-F DELIMITED BY SIZE
                           INTO LOG-DATA(LOG-IDX)
           END-READ
       END-PERFORM
       CLOSE LOG-FILE

       IF FOUND NOT = "Y"
       DISPLAY "No matching for the given Member ID and Book ID."
       STOP RUN
       END-IF

       OPEN OUTPUT LOG-FILE
       PERFORM VARYING LOG-IDX FROM 1 BY 1 UNTIL LOG-IDX > 100
           IF LOG-DATA(LOG-IDX) NOT = SPACE
               MOVE LOG-DATA(LOG-IDX) TO LOG-REC
               WRITE LOG-REC
           END-IF
       END-PERFORM
       CLOSE LOG-FILE

       MOVE 0 TO BK-TOTAL
       MOVE 1 TO BK-IDX
       OPEN INPUT BOOK-FILE
       MOVE "N" TO FILE-END
       PERFORM UNTIL FILE-END = "Y"
           READ BOOK-FILE
               AT END
                   MOVE "Y" TO FILE-END
               NOT AT END
                   UNSTRING BOOK-REC DELIMITED BY ","
                       INTO BK-ID(BK-IDX), BK-NAME(BK-IDX),
                            BK-AUTHOR(BK-IDX), BK-COUNT(BK-IDX),
                            BK-GENRE(BK-IDX)
                   IF BK-ID(BK-IDX) = WS-BOOK-ID
                       COMPUTE BK-COUNT(BK-IDX) = BK-COUNT(BK-IDX) + 1
                   END-IF
                   ADD 1 TO BK-IDX
                   ADD 1 TO BK-TOTAL
       END-PERFORM
       CLOSE BOOK-FILE

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

       DISPLAY "Book return complete. Thank you.".
       EXIT PROGRAM.
       STOP RUN.
       END PROGRAM ReturnBook.
