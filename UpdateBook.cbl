      ******************************************************************
      * Author: Kaung Khant Nyein
      * Date: 10/7/25
      * Purpose: Update The Data
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UpdateBook.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOK-FILE ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOK-FILE.
       01  BOOK-RECORD        PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-BOOK-ID         PIC X(5).
       01  WS-BOOK-NAME       PIC X(30).
       01  WS-BOOK-AUTHOR     PIC X(30).
       01  WS-BOOK-GENRE      PIC X(30).
       01  WS-BOOK-COUNT      PIC 9(2).
       01  TEMP-ID            PIC X(5).
       01  TEMP-NAME          PIC X(30).
       01  TEMP-AUTHOR        PIC X(30).
       01  TEMP-GENRE         PIC X(30).
       01  TEMP-COUNT         PIC 9(2).
       01  INPUT-STR          PIC X(200).
       01  FILE-END           PIC X VALUE 'N'.
       01  FOUND-FLAG         PIC X VALUE 'N'.
       01  USER-ID            PIC X(5).
       01  NEW-NAME           PIC X(30).
       01  NEW-AUTHOR         PIC X(30).
       01  NEW-GENRE          PIC X(30).
       01  NEW-COUNT          PIC 9(2).
       01  LINE-TABLE.
           05  LINE-ENTRY OCCURS 100 TIMES.
               10  LINE-CONTENT  PIC X(200).

       77  LINE-ID            PIC 9(3) VALUE 1.
       77  I                  PIC 9(3) VALUE 1.

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
       MAIN-PROCEDURE.
           OPEN INPUT BOOK-FILE
           PERFORM UNTIL FILE-END = 'Y'
           READ BOOK-FILE
               AT END
                   MOVE 'Y' TO FILE-END
               NOT AT END
                   MOVE BOOK-RECORD TO INPUT-STR
                   UNSTRING INPUT-STR DELIMITED BY ","
                       INTO TEMP-ID, TEMP-NAME, TEMP-AUTHOR,
                       TEMP-COUNT,TEMP-GENRE
                   MOVE INPUT-STR TO LINE-CONTENT(LINE-ID)
                   ADD 1 TO LINE-ID
           END-READ
           END-PERFORM
           CLOSE BOOK-FILE

       DISPLAY "Enter Book ID to update: "
       ACCEPT USER-ID

       PERFORM VARYING I FROM 1 BY 1 UNTIL I >= LINE-ID OR
       FOUND-FLAG = 'Y'
           MOVE LINE-CONTENT(I) TO INPUT-STR
           UNSTRING INPUT-STR DELIMITED BY ","
               INTO TEMP-ID, TEMP-NAME, TEMP-AUTHOR,
               TEMP-COUNT,TEMP-GENRE
           IF TEMP-ID = USER-ID
               DISPLAY "Current Name  : " TEMP-NAME
               DISPLAY "Current Author: " TEMP-AUTHOR
               DISPLAY "Current Genre : " TEMP-GENRE
               DISPLAY "Current Count : " TEMP-COUNT
               DISPLAY "Enter new name (or press ENTER to skip): "
               ACCEPT NEW-NAME
               IF NEW-NAME = SPACES THEN
                   MOVE TEMP-NAME TO NEW-NAME
               END-IF
               DISPLAY "Enter new author (or press ENTER to skip): "
               ACCEPT NEW-AUTHOR
               IF NEW-AUTHOR = SPACES THEN
                   MOVE TEMP-AUTHOR TO NEW-AUTHOR
               END-IF
               DISPLAY "Enter new genre (or press ENTER to skip): "
               ACCEPT NEW-GENRE
               IF NEW-GENRE = SPACES THEN
                   MOVE TEMP-GENRE TO NEW-GENRE
               END-IF
               DISPLAY "Enter new count (or press ENTER to skip): "
               ACCEPT NEW-COUNT
               IF NEW-COUNT = ZERO THEN
                   MOVE TEMP-COUNT TO NEW-COUNT
               END-IF
               STRING TEMP-ID DELIMITED BY SIZE ","
                      NEW-NAME DELIMITED BY SIZE ","
                      NEW-AUTHOR DELIMITED BY SIZE ","
                      NEW-GENRE DELIMITED BY SIZE ","
                      NEW-COUNT DELIMITED BY SIZE
                   INTO LINE-CONTENT(I)
               MOVE 'Y' TO FOUND-FLAG
           END-IF
       END-PERFORM

       IF FOUND-FLAG = 'N'
           DISPLAY "Book ID not found."
           STOP RUN
       END-IF

       OPEN OUTPUT BOOK-FILE
       PERFORM VARYING I FROM 1 BY 1 UNTIL I >= LINE-ID
           MOVE LINE-CONTENT(I) TO BOOK-RECORD
           WRITE BOOK-RECORD
       END-PERFORM
       CLOSE BOOK-FILE

       DISPLAY "Book info updated successfully.".
      *>  STOP RUN.

       END PROGRAM UpdateBook.
