      ******************************************************************
      * Author: Ei Khine Moe
      * Date: 8/7/2025
      * Purpose: Add New Book
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddNewBook.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BookFile ASSIGN TO '../books.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BookFile.
       01  BookRecord               PIC X(200).

       WORKING-STORAGE SECTION.
       01  book_records.
           05  book_id             PIC 9(5).
           05  book_name           PIC X(30).
           05  book_author         PIC X(30).
           05  book_count          PIC 9(2).
           05  book_genre          PIC X(30).

       01  ws-book-line            PIC X(200).
       01  add-book-confirm-choice PIC 9(1).
       01  last-book-id            PIC 9(5) VALUE 0.
       01  eof-flag                PIC X VALUE 'N'.

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.
           MAIN-PROCEDURE.
           OPEN INPUT BookFile
           PERFORM UNTIL eof-flag = 'Y'
               READ BookFile
                   AT END
                       MOVE 'Y' TO eof-flag
                   NOT AT END
                   UNSTRING BookRecord DELIMITED BY "," INTO book_id
                   MOVE book_id TO last-book-id
               END-READ
           END-PERFORM
           CLOSE BookFile

           ADD 1 TO last-book-id
           MOVE last-book-id TO book_id

           DISPLAY "book_ID__"book_id

           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "*         Add New Book to Library           *"
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"

           DISPLAY "Enter Book Name     : " ACCEPT book_name
           DISPLAY "Enter Author Name   : " ACCEPT book_author
           DISPLAY "Enter Book Count    : " ACCEPT book_count
           DISPLAY "Enter Genre         : " ACCEPT book_genre

           DISPLAY "*------------------------------------------*"
           DISPLAY "Enter 1 to Save, 0 to Cancel: "
           ACCEPT add-book-confirm-choice

           IF add-book-confirm-choice = 1 THEN
      *>          STRING
      *>              book_id           DELIMITED BY SIZE
      *>              ","               DELIMITED BY SIZE
      *>              FUNCTION TRIM(book_name)    DELIMITED BY SIZE
      *>              ","               DELIMITED BY SIZE
      *>              FUNCTION TRIM(book_author)  DELIMITED BY SIZE
      *>              ","               DELIMITED BY SIZE
      *>              book_count        DELIMITED BY SIZE
      *>              ","               DELIMITED BY SIZE
      *>              FUNCTION TRIM(book_genre)   DELIMITED BY SIZE
      *>              INTO ws-book-line
      *>          END-STRING

               IF book_id = 1 THEN
                   MOVE 20001 TO book_id
                   OPEN OUTPUT BookFile
               ELSE
                   OPEN EXTEND BookFile
               END-IF
      *>          OPEN EXTEND BookFile
               STRING
                   book_id           DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_name)    DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_author)  DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   book_count        DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_genre)   DELIMITED BY SIZE
                   INTO ws-book-line
               END-STRING
               MOVE ws-book-line TO BookRecord
               WRITE BookRecord
               CLOSE BookFile

               DISPLAY "*------------------------------------------*"
               DISPLAY "Book successfully added to books.csv."
               DISPLAY "Book ID   : " book_id
               DISPLAY "Book Name : " book_name
               DISPLAY "*------------------------------------------*"
           ELSE
               DISPLAY "Book entry cancelled."
           END-IF.
      *>      STOP RUN.
           END PROGRAM AddNewBook.
