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
           05 FILLER   PIC X(12) VALUE "BOOK-ID     ".
           05 FILLER   PIC X(30) VALUE "BOOK NAME                    ".
           05 FILLER   PIC X(26) VALUE "AUTHOR                    ".
           05 FILLER   PIC X(8)  VALUE "COUNT   ".
           05 FILLER   PIC X(15) VALUE "GENRE          ".


       01 BOOK-DETAIL.
           05 book_id              PIC X(5).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_name            PIC X(30).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_author          PIC X(30).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_count           PIC 9(2).
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 book_genre           PIC X(30).

       01 BOOK-DISPLAY-LINE.
      *>      05 FILLER                PIC X(3)  VALUE SPACES.
           05 DISP-BOOK-ID          PIC X(5).
           05 FILLER                PIC X(7)  VALUE SPACES.
           05 DISP-BOOK-NAME        PIC X(28).
           05 FILLER                PIC X(2)  VALUE SPACES.
           05 DISP-BOOK-AUTHOR      PIC X(25).
           05 FILLER                PIC X(1)  VALUE SPACES.
           05 DISP-BOOK-COUNT       PIC Z9.
           05 FILLER                PIC X(6)  VALUE SPACES.
           05 DISP-BOOK-GENRE       PIC X(20).


       01 DECOR-LINE              PIC X(95) VALUE ALL '*-'.

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).

       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-LOGIC
           EXIT PROGRAM
           STOP RUN.

       MAIN-LOGIC.
           OPEN INPUT BOOK-FILE
           IF BOOK-STATUS NOT = '00'
               DISPLAY "ERROR OPENING BOOKS FILE: " BOOK-STATUS
           ELSE
               DISPLAY " "
               DISPLAY "LIST OF ALL BOOKS"
               DISPLAY DECOR-LINE
               DISPLAY BOOK-HEADER
               DISPLAY DECOR-LINE

               PERFORM UNTIL BOOK-STATUS = '10'
                   READ BOOK-FILE
                       AT END
                           DISPLAY DECOR-LINE
                       NOT AT END
                           UNSTRING BOOK-RECORD DELIMITED BY ','
                               INTO book_id, book_name, book_author,
                               book_count, book_genre
      *>                      DISPLAY BOOK-DETAIL

                           MOVE book_id       TO DISP-BOOK-ID
                           MOVE book_name     TO DISP-BOOK-NAME
                           MOVE book_author   TO DISP-BOOK-AUTHOR
                           MOVE book_count    TO DISP-BOOK-COUNT
                           MOVE book_genre    TO DISP-BOOK-GENRE
                           DISPLAY BOOK-DISPLAY-LINE

                   END-READ
               END-PERFORM
      *>          DISPLAY DECOR-LINE
               CLOSE BOOK-FILE
           END-IF
           GOBACK.

       END PROGRAM ListAllBooks.
