      ******************************************************************
      * Author : Ei Ei Thant
      * Purpose: Listing All Books (Subprogram)
      * To fix : Paging Needed
      * Modified by HL(7/10/25)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ListAllBooks.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOK-FILE ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS BOOK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD BOOK-FILE.
       01 BOOK-RECORD               PIC X(200).

       WORKING-STORAGE SECTION.
       01 BOOK-STATUS               PIC XX.
       01 BOOK-HEADER.
           05 FILLER                PIC X(10) VALUE "BOOK ID".
           05 FILLER                PIC X(2)  VALUE SPACES.
           05 FILLER                PIC X(30) VALUE "BOOK NAME".
           05 FILLER                PIC X(2)  VALUE SPACES.
           05 FILLER                PIC X(25) VALUE "AUTHOR".
           05 FILLER                PIC X(2)  VALUE SPACES.
           05 FILLER                PIC X(5)  VALUE "COUNT".
           05 FILLER                PIC X(4)  VALUE SPACES.
           05 FILLER                PIC X(15) VALUE "GENRE".

       01 BOOK-DETAIL.
           05 book_id              PIC X(10).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_name            PIC X(30).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_author          PIC X(25).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_count           PIC 9(3).
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 book_genre           PIC X(15).

       01 HEADER-LINE              PIC X(100) VALUE ALL '-'.

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-LOGIC
           EXIT PROGRAM.
       MAIN-LOGIC.
           OPEN INPUT BOOK-FILE
           IF BOOK-STATUS NOT = '00'
               DISPLAY "ERROR OPENING BOOKS FILE: " BOOK-STATUS
           ELSE
               DISPLAY " "
               DISPLAY "LIST OF ALL BOOKS"
               DISPLAY HEADER-LINE
               DISPLAY BOOK-HEADER
               DISPLAY HEADER-LINE

               PERFORM UNTIL BOOK-STATUS = '10'
                   READ BOOK-FILE INTO BOOK-RECORD
                       AT END
                           DISPLAY " "
                       NOT AT END
                           UNSTRING BOOK-RECORD DELIMITED BY ','
                               INTO book_id, book_name, book_author,
                               book_count, book_genre
                           DISPLAY BOOK-DETAIL
                   END-READ
               END-PERFORM
               DISPLAY HEADER-LINE
               CLOSE BOOK-FILE
           END-IF
           GOBACK.

       END PROGRAM ListAllBooks.
