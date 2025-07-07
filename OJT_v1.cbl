      ******************************************************************
      * Author:
      * Date:7/7/2025
      * Purpose:container
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. Main-Container.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01  WS-CHOICE PIC 9.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           DISPLAY "1. Create New Member"
           DISPLAY "2. Search Books"
           DISPLAY "3. Issuance Book(s)"
           DISPLAY "4. Return Book(s)"
           DISPLAY "5. Show Logs"
           DISPLAY "6. Update Books"
           DISPLAY "Enter your choice (1-6): "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM CREATE-NEW-MEMBER
               WHEN 2
                   PERFORM SEARCH-BOOK
               WHEN 3
                   PERFORM ISSUANCE-BOOK
               WHEN 4
                   PERFORM RETURN-BOOK
               WHEN 5
                   PERFORM SHOW-LOGS
               WHEN 6
                   PERFORM UPDATE-BOOK
               WHEN OTHER
                   DISPLAY "INVALID CHOICE"

           END-EVALUATE.

            STOP RUN.

      ** add other procedures here


       CREATE-NEW-MEMBER.
           DISPLAY "This is member registration display.".

       SEARCH-BOOK.
           DISPLAY "This is book search display.".

       ISSUANCE-BOOk.
           DISPLAY "This is book issuance display.".

       RETURN-BOOK.
           DISPLAY "This is book return display.".

       SHOW-LOGS.
           DISPLAY "This is logs showing display.".

       UPDATE-BOOK.
           DISPLAY "This is book update display.".

       END PROGRAM Main-Container.
