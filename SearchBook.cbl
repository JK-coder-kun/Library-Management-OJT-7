      ******************************************************************
      * Author : Ei Ei Thant
      * Purpose: Search Books (Subprogram)
      * To fix : OK
      * Modified by HL(7/10/25)
      * Modified by KK(7/22/25)
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. SearchBook.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT BOOK-FILE ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS BOOK-STATUS.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *> -----------------
       FD BOOK-FILE.
       01 BOOK-RECORD             PIC X(200).

       WORKING-STORAGE SECTION.
       01 BOOK-STATUS             PIC XX.

       01 SEARCH-CRITERIA.
           05 SC-ID               PIC X(5).
           05 SC-NAME             PIC X(30).
           05 SC-AUTHOR           PIC X(35).
           05 SC-GENRE            PIC X(30).

       01 BOOK-DETAIL.
           05 BOOK-ID             PIC X(5).
           05 FILLER              PIC X(7) VALUE SPACES.
           05 BOOK-NAME           PIC X(30).
           05 FILLER              PIC X(3) VALUE SPACES.
           05 BOOK-AUTHOR         PIC X(30).
           05 FILLER              PIC X(3) VALUE SPACES.
           05 BOOK-COUNT          PIC 9(2).
           05 FILLER              PIC X(5) VALUE SPACES.
           05 BOOK-GENRE          PIC X(30).

       01 MATCH-FOUND             PIC X VALUE "N".
           88 BOOK-MATCH-FOUND    VALUE "Y".
           88 NO-BOOK-MATCH       VALUE "N".

       01 found_flag PIC X value 'N'.
       01 HEADER-LINE             PIC X(96) VALUE ALL '-'.

       01 BOOK-HEADER.
           05 FILLER              PIC X(7) VALUE "BOOK ID".
           05 FILLER              PIC X(5)  VALUE SPACES.
           05 FILLER              PIC X(30) VALUE "BOOK NAME".
           05 FILLER              PIC X(3)  VALUE SPACES.
           05 FILLER              PIC X(30) VALUE "AUTHOR".
           05 FILLER              PIC X(3)  VALUE SPACES.
           05 FILLER              PIC X(5)  VALUE "COUNT".
           05 FILLER              PIC X(2)  VALUE SPACES.
           05 FILLER              PIC X(15) VALUE "GENRE".

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).

       PROCEDURE DIVISION USING USER-CHOICE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
           PERFORM MAIN-PARAGRAPH
           EXIT PROGRAM.
           STOP RUN.

       MAIN-PARAGRAPH.
           MOVE SPACES TO SEARCH-CRITERIA
           SET NO-BOOK-MATCH TO TRUE.
           MOVE 'N' TO found_flag
           DISPLAY " "
           DISPLAY "SEARCH BOOKS BY CRITERIA (leave blank to skip any):"
           DISPLAY "Enter Book ID: "     ACCEPT SC-ID
           DISPLAY "Enter Book Name: "   ACCEPT SC-NAME
           DISPLAY "Enter Author: "      ACCEPT SC-AUTHOR
           DISPLAY "Enter Genre: "       ACCEPT SC-GENRE.

          *> -----------------

           OPEN INPUT BOOK-FILE
           IF BOOK-STATUS NOT = "00"
               DISPLAY "Error opening file. Status: " BOOK-STATUS
               EXIT PARAGRAPH
           END-IF

           DISPLAY "======================"
           DISPLAY "    SEARCH RESULTS"
           DISPLAY "======================"
      *>      DISPLAY HEADER-LINE
           DISPLAY " "
           DISPLAY BOOK-HEADER
           DISPLAY HEADER-LINE

           PERFORM UNTIL BOOK-STATUS = "10"
               READ BOOK-FILE
                   AT END CONTINUE
                   NOT AT END
                       UNSTRING BOOK-RECORD DELIMITED BY ","
                           INTO BOOK-ID, BOOK-NAME, BOOK-AUTHOR,
                                BOOK-COUNT, BOOK-GENRE
                       PERFORM CHECK-MATCH

                       IF BOOK-MATCH-FOUND
                           DISPLAY BOOK-DETAIL
                           SET BOOK-MATCH-FOUND TO TRUE
                       END-IF

               END-READ
           END-PERFORM

           IF found_flag = 'N' THEN
               DISPLAY "No books matched your search criteria."
           END-IF

           DISPLAY HEADER-LINE
           CLOSE BOOK-FILE.

      *-----------------------------------------------------------------
      *>      STOP RUN.

       CHECK-MATCH.
           SET NO-BOOK-MATCH TO TRUE
           IF (SC-ID = SPACES OR SC-ID = BOOK-ID) AND
              (SC-NAME = SPACES OR SC-NAME = BOOK-NAME) AND
              (SC-AUTHOR = SPACES OR SC-AUTHOR = BOOK-AUTHOR) AND
              (SC-GENRE = SPACES OR SC-GENRE = BOOK-GENRE)
               SET BOOK-MATCH-FOUND TO TRUE
               MOVE 'Y' TO found_flag
           END-IF.
