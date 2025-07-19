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
       01  TMP-LOG-REC     PIC x(200).
       FD  BOOK-FILE.
       01  BOOK-REC        PIC X(200).
       FD  FINE-FILE.
       01  FINE-REC        PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-MEMBER-ID     PIC X(5).
       01  WS-BOOK-ID       PIC X(5).
       01  WS-RETURN-DATE   PIC X(10).
       01  SYS-DATE         PIC 9(8).
       01  SYS-DD           PIC X(2).
       01  SYS-MM           PIC X(2).
       01  SYS-YYYY         PIC X(4).
       01  WS-RETURN-INT    PIC 9(8).
       01  END-INT          PIC 9(8).
       01  DIFF-DAYS        PIC 9(3).
       01  FINE-AMOUNT      PIC 9(5).
       01  FOUND            PIC X VALUE "N".
       01  IDX              PIC 9(3).
       01  CNT              PIC 9(3) VALUE 0.
       01  BK-CNT           PIC 9(3) VALUE 0.
       01  FILE-STATUS      PIC X(1).
       01  LOG-TABLE.
           05 LOG-ENTRY OCCURS 1000 TIMES.
               10 TR-ID      PIC X(5).
               10 MB-ID      PIC X(5).
               10 BK-ID      PIC X(5).
               10 ST-DATE    PIC X(10).
               10 ED-DATE    PIC X(10).
               10 DUE-FLAG   PIC X(3).
               10 RTN-DATE   PIC X(10).

       01  BOOK-TABLE.
           05 BOOK-ENTRY OCCURS 100 TIMES.
               10 BK-ID-TAB     PIC X(5).
               10 BK-NAME       PIC X(30).
               10 BK-AUTHOR     PIC X(30).
               10 BK-COUNT      PIC 99.
               10 BK-GENRE      PIC X(30).


       01  CURR-ID-X        PIC X(5).
       01  CURR-ID-N        PIC 9(5).
       01  MAX-FINE-ID      PIC 9(5) VALUE 0.
       01  NEW-FINE-ID      PIC 9(5).
       01  CONFIRM          PIC X.
       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PARAGRAPH
           EXIT PROGRAM.
       MAIN-PARAGRAPH.
       DISPLAY "Enter Member ID: "
       ACCEPT WS-MEMBER-ID
       DISPLAY "Enter Book ID: "
       ACCEPT WS-BOOK-ID

       ACCEPT SYS-DATE FROM DATE YYYYMMDD
       MOVE SYS-DATE(1:4) TO SYS-YYYY
       MOVE SYS-DATE(5:2) TO SYS-MM
       MOVE SYS-DATE(7:2) TO SYS-DD
       STRING SYS-DD DELIMITED BY SIZE "-"
       SYS-MM DELIMITED BY SIZE "-"
       SYS-YYYY DELIMITED BY SIZE
           INTO WS-RETURN-DATE
       STRING SYS-YYYY DELIMITED BY SIZE
       SYS-MM DELIMITED BY SIZE
       SYS-DD DELIMITED BY SIZE
           INTO WS-RETURN-INT

       OPEN INPUT LOG-FILE
       PERFORM UNTIL CNT = 100
           READ LOG-FILE
               AT END
                   EXIT PERFORM
               NOT AT END
                   ADD 1 TO CNT
                   UNSTRING LOG-REC DELIMITED BY ","
                       INTO TR-ID(CNT) MB-ID(CNT) BK-ID(CNT)
                 ST-DATE(CNT) ED-DATE(CNT) DUE-FLAG(CNT) RTN-DATE(CNT)
           END-READ
       END-PERFORM
       CLOSE LOG-FILE

       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > CNT
           IF MB-ID(IDX) = WS-MEMBER-ID AND
              BK-ID(IDX) = WS-BOOK-ID AND
              RTN-DATE(IDX) = SPACES
               MOVE WS-RETURN-DATE TO RTN-DATE(IDX)
               MOVE "Y" TO FOUND

               IF DUE-FLAG(IDX) = "YES"
                   MOVE ED-DATE(IDX)(7:4) TO SYS-YYYY
                   MOVE ED-DATE(IDX)(1:2) TO SYS-DD
                   MOVE ED-DATE(IDX)(4:2) TO SYS-MM
                   STRING SYS-YYYY DELIMITED BY SIZE SYS-MM
                   DELIMITED BY SIZE SYS-DD DELIMITED BY SIZE
                       INTO END-INT
           MOVE FUNCTION INTEGER-OF-DATE(WS-RETURN-INT) TO WS-RETURN-INT
           MOVE FUNCTION INTEGER-OF-DATE(END-INT) TO END-INT
                   COMPUTE DIFF-DAYS = WS-RETURN-INT - END-INT
                   COMPUTE FINE-AMOUNT = DIFF-DAYS * 1000
                   DISPLAY "Overdue by ", DIFF-DAYS, " days."
                   DISPLAY "Fine: ", FINE-AMOUNT, " MMK"
                   DISPLAY "Confirm return and pay fine (Y/N)?"
                   ACCEPT CONFIRM
                   IF CONFIRM NOT = "Y"
                       DISPLAY "Return canceled." STOP RUN
                   END-IF

                   OPEN INPUT FINE-FILE
                   PERFORM UNTIL FILE-STATUS = "Y"
                       READ FINE-FILE
                           AT END MOVE "Y" TO FILE-STATUS
                           NOT AT END
                   UNSTRING FINE-REC DELIMITED BY "," INTO CURR-ID-X
                               MOVE CURR-ID-X TO CURR-ID-N
                               IF CURR-ID-N > MAX-FINE-ID
                                   MOVE CURR-ID-N TO MAX-FINE-ID
                       END-READ
                   END-PERFORM
                   CLOSE FINE-FILE
                   COMPUTE NEW-FINE-ID = MAX-FINE-ID + 1
                   OPEN EXTEND FINE-FILE
                   STRING NEW-FINE-ID DELIMITED BY SIZE ","
                   TR-ID(IDX) DELIMITED BY SIZE ","
                   MB-ID(IDX) DELIMITED BY SIZE ","
                   DIFF-DAYS DELIMITED BY SIZE ","
                   FINE-AMOUNT DELIMITED BY SIZE
                       INTO FINE-REC
                   WRITE FINE-REC
                   CLOSE FINE-FILE
               END-IF
               EXIT PERFORM
           END-IF
       END-PERFORM

       IF FOUND NOT = "Y"
           DISPLAY "No matching record found."
           STOP RUN
       END-IF

       OPEN OUTPUT LOG-FILE
       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > CNT
           STRING TR-ID(IDX) DELIMITED BY SIZE","
           MB-ID(IDX) DELIMITED BY SIZE ","
           BK-ID(IDX) DELIMITED BY SIZE ","
           ST-DATE(IDX) DELIMITED BY SIZE ","
           ED-DATE(IDX) DELIMITED BY SIZE ","
           DUE-FLAG(IDX) DELIMITED BY SIZE ","
           RTN-DATE(IDX) DELIMITED BY SIZE
               INTO TMP-LOG-REC
            WRITE TMP-LOG-REC
       END-PERFORM
       CLOSE LOG-FILE

       OPEN INPUT BOOK-FILE
       PERFORM UNTIL BK-CNT = 100
           READ BOOK-FILE
               AT END EXIT PERFORM
               NOT AT END
                   ADD 1 TO BK-CNT
                   UNSTRING BOOK-REC DELIMITED BY ","
                       INTO BK-ID-TAB(BK-CNT),BK-NAME(BK-CNT),
                       BK-AUTHOR(BK-CNT), BK-COUNT(BK-CNT),
                       BK-GENRE(BK-CNT)
       END-PERFORM
       CLOSE BOOK-FILE

       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > BK-CNT
           IF BK-ID-TAB(IDX) = WS-BOOK-ID
               ADD 1 TO BK-COUNT(IDX)
               EXIT PERFORM
           END-IF
       END-PERFORM

       OPEN OUTPUT BOOK-FILE
       PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > BK-CNT
           STRING BK-ID-TAB(IDX) DELIMITED BY SIZE ","
           BK-NAME(IDX) DELIMITED BY SIZE ","
           BK-AUTHOR(IDX) DELIMITED BY SIZE ","
           BK-COUNT(IDX) DELIMITED BY SIZE ","
           BK-GENRE(IDX) DELIMITED BY SIZE
               INTO BOOK-REC
           WRITE BOOK-REC
       END-PERFORM
       CLOSE BOOK-FILE

       DISPLAY "Book returned successfully.".
       END PROGRAM ReturnBook.
