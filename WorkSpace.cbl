      ******************************************************************
      * Author:HL
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
       FILE-CONTROL.
           SELECT MemberFile ASSIGN TO 'member.csv'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD  MemberFile.
       01  MemberRecord      PIC X(100).

       WORKING-STORAGE SECTION.
      *-----------------------
       01  WS-CHOICE         PIC 9.
       01  WS-CSV-LINE       PIC X(100).
       *> -------------------------------------------------------

       01  WS-FileStatus     PIC XX.

       01 member_record.
           05  member_id         PIC 9(5).
           05  member_name       PIC X(30).
           05  member_gender     PIC X.
           05  member_email      PIC X(35).
           05  member_address    PIC X(50).
           05  memeber_flag      PIC X(10).

       01  cm_choice PIC 9(1).
       01  continue_flag     PIC X VALUE "Y".
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
           DISPLAY "This is member registration display."

           OPEN EXTEND MemberFile
           DISPLAY "Enter Name: "    ACCEPT member_name
           DISPLAY "Enter Gender: "  ACCEPT member_gender
           DISPLAY "Enter Email: "   ACCEPT member_email
           DISPLAY "Enter Address: " ACCEPT member_address

           DISPLAY "Enter 1. to create, 0. to exit:  "
           ACCEPT cm_choice
           *> member id display kyan

           STRING
           member_id DELIMITED BY SIZE "," DELIMITED BY SIZE
           member_name DELIMITED BY SIZE "," DELIMITED BY SIZE
           member_gender DELIMITED BY SIZE "," DELIMITED BY SIZE
           member_email DELIMITED BY SIZE "," DELIMITED BY SIZE
           member_address DELIMITED BY SIZE "," DELIMITED BY SIZE
           INTO WS-CSV-Line

           CLOSE MemberFile
           DISPLAY "Member data written to members.csv".



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





               *> MOVE WS-CSV-Line TO MemberRecord
               *> WRITE MemberRecord
