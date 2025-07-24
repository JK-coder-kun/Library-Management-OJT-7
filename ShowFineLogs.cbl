      ******************************************************************
      * Author:Htay Lwin
      * Date:7/16/2025
      * Purpose: Display the logs of fine members
      * Tectonics: cobc
      * Fix: Zero suppression in the numerical values
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ShowFineLogs.
      *-----------------------
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT FineFile    ASSIGN TO "../fine.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LogFile     ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MemberFile  ASSIGN TO "../members.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BookFile    ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD FineFile.
       01 FineRecord         PIC X(200).

       FD LogFile.
       01 LogRecord          PIC X(200).

       FD MemberFile.
       01 MemberRecord       PIC X(200).

       FD BookFile.
       01 BookRecord         PIC X(200).

       WORKING-STORAGE SECTION.
      *-----------------------
       01 EOF-Fine           PIC X VALUE 'N'.
       01 match_member_name  PIC X(30) VALUE SPACES.
       01 match_book_name    PIC X(30) VALUE SPACES.

       01 Fine.
           05 f_fine_id      PIC X(5).
           05 f_tran_id      PIC X(5).
           05 f_member_id    PIC X(5).
           05 f_due_days     PIC 9(3).
           05 f_amount       PIC 9(5).

       01 Log.
           05 l_tran_id       PIC X(5).
           05 l_member_id     PIC X(5).
           05 l_book_id       PIC X(5).
           05 l_start_date    PIC X(10).
           05 l_end_date      PIC X(10).
           05 l_due_flag      PIC X(3).
           05 l_return_date   PIC X(10).

       01 Member.
           05 m_member_id     PIC X(5).
           05 m_name          PIC X(30).
           05 m_email         PIC X(35).
           05 m_address       PIC X(50).
           05 m_gender        PIC X(1).
           05 m_flag          PIC X(8).

       01 Book.
           05 b_book_id       PIC X(5).
           05 b_name          PIC X(30).
           05 b_author        PIC X(30).
           05 b_count         PIC X(2).
           05 b_genre         PIC X(30).

       01 DISPLAY-HEADER.
           05 FILLER PIC X(9)   VALUE "FineID   ".
           05 FILLER PIC X(18)  VALUE "Member Name       ".
           05 FILLER PIC X(30)  VALUE "Book Name                    ".
           05 FILLER PIC X(12)  VALUE "Due Days    ".
           05 FILLER PIC X(8)   VALUE " Amount".


       01 deco-line           PIC x(77) value all "*-".
       01 f_due_days_disp     PIC Z(3).
       01 f_amount_disp       PIC Z(6).
       01  choice      PIC X.
       01  counter PIC 999 value 0.

       LINKAGE SECTION.
      *-----------------------
       01 USER-CHOICE PIC 9(2).

       PROCEDURE DIVISION USING USER-CHOICE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM
       STOP RUN.

      *-----------------------------------------------------------------

       MAIN-PROCEDURE.
           MOVE 'N' TO EOF-Fine
           DISPLAY " "
           DISPLAY "FINED MEMBERS' LOGS"
           DISPLAY "==================="
           DISPLAY " "
           DISPLAY DISPLAY-HEADER

           OPEN INPUT FineFile LogFile MemberFile BookFile
           DISPLAY deco-line
           MOVE 0 TO counter
           PERFORM UNTIL EOF-Fine = 'Y'
            READ FineFile
                AT END
                    MOVE 'Y' TO EOF-Fine
                NOT AT END

                        UNSTRING FineRecord DELIMITED BY ","
                        INTO f_fine_id, f_tran_id, f_member_id,
                             f_due_days, f_amount

                        PERFORM FETCH-LOG-DETAILS
                    PERFORM FETCH-MEMBER-NAME
                    PERFORM FETCH-BOOK-NAME

                        MOVE f_due_days TO f_due_days_disp
                    MOVE f_amount   TO f_amount_disp

                        DISPLAY f_fine_id "    "
                            match_member_name(1:18)
                            match_book_name(1:30)
                            "  "
                            f_due_days_disp
                            "        "
                            f_amount_disp

                        IF counter >= 10 THEN
                          MOVE 0 TO counter
                          DISPLAY "Press Enter (To Show Next Page)"
                               " or Q(To Quit):"
                          ACCEPT choice
                          IF choice = "Q" OR choice = "q" THEN
                               MOVE 'Y' TO EOF-Fine
                          END-IF
                        END-IF

            END-READ
           END-PERFORM

               DISPLAY deco-line
           CLOSE FineFile LogFile MemberFile BookFile.

      *-----------------------------------------------------------------
       FETCH-LOG-DETAILS.
           MOVE SPACES TO l_tran_id l_member_id l_book_id
           CLOSE LogFile
           OPEN INPUT LogFile

           PERFORM UNTIL l_tran_id = f_tran_id
               READ LogFile
                   AT END EXIT PERFORM
                   NOT AT END

                       UNSTRING LogRecord DELIMITED BY ","
                           INTO l_tran_id, l_member_id, l_book_id,
                                l_start_date, l_end_date, l_due_flag,
                                l_return_date
                       END-UNSTRING

                       IF l_tran_id = f_tran_id
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------
       FETCH-MEMBER-NAME.
           MOVE SPACES TO match_member_name
           CLOSE MemberFile
           OPEN INPUT MemberFile
           PERFORM UNTIL match_member_name NOT = SPACES
               READ MemberFile
                   AT END
                       MOVE "NOT FOUND" TO match_member_name
                       EXIT PERFORM
                   NOT AT END
                       UNSTRING MemberRecord DELIMITED BY ','
                       INTO
                           m_member_id, m_name, m_email,
                           m_address, m_gender, m_flag
                       END-UNSTRING

                       IF l_member_id = m_member_id
                           MOVE m_name TO match_member_name
                       END-IF
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------
       FETCH-BOOK-NAME.
           MOVE SPACES TO match_book_name
           CLOSE BookFile
           OPEN INPUT BookFile
           PERFORM UNTIL match_book_name NOT = SPACES
               READ BookFile
                   AT END
                       MOVE "NOT FOUND" TO match_book_name
                       EXIT PERFORM
                   NOT AT END

                       UNSTRING BookRecord DELIMITED BY ","
                           INTO b_book_id, b_name, b_author,
                                b_count, b_genre
                       END-UNSTRING

                       IF l_book_id = b_book_id
                           MOVE b_name TO match_book_name
                       END-IF
               END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       END PROGRAM ShowFineLogs.
