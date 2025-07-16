      ******************************************************************
      * Author:Htay Lwin
      * Date:7/15/2025
      * Purpose: Shows logs of borrowing/return records
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ShowHistoryLogs.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
           SELECT LogFile ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MemberFile ASSIGN TO "../members.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BookFile ASSIGN TO "../books.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD LogFile.
       01 LogRecord          PIC X(200).

       FD MemberFile.
       01 MemberRecord       PIC X(200).

       FD BookFile.
       01 BookRecord         PIC X(200).

       WORKING-STORAGE SECTION.
      *-----------------------
       01 EOF-Log            PIC X VALUE 'N'.
       01 EOF-Member         PIC X VALUE 'N'.
       01 EOF-Book           PIC X VALUE 'N'.
       01 match_member_name  PIC X(30) VALUE SPACES.
       01 match_book_name    PIC X(30) VALUE SPACES.

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
           05 FILLER PIC X(9)   VALUE "TransID  ".
           05 FILLER PIC X(18)  VALUE "Member Name       ".
           05 FILLER PIC X(30)  VALUE "Book Name                    ".
           05 FILLER PIC X(12)  VALUE "Start Date  ".
           05 FILLER PIC X(3)   VALUE "Due".
           05 FILLER PIC X(1)   VALUE " ".
           05 FILLER PIC X(10)  VALUE "ReturnDate".

       01 deco-line           PIC x(83) value all "*-".

       LINKAGE SECTION.
      *-----------------------
       01 USER-CHOICE PIC 9(2).


       PROCEDURE DIVISION USING USER-CHOICE.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

           DISPLAY " "
           DISPLAY " Issuance Records "
           DISPLAY "=================="
           DISPLAY " "
           DISPLAY DISPLAY-HEADER

           OPEN INPUT LogFile MemberFile BookFile
           READ LogFile
               AT END MOVE 'Y' TO EOF-Log
           END-READ

           DISPLAY deco-line

           PERFORM UNTIL EOF-Log = 'Y'
               UNSTRING LogRecord DELIMITED BY ","
                   INTO l_tran_id, l_member_id, l_book_id,
                        l_start_date, l_end_date, l_due_flag,
                        l_return_date
               END-UNSTRING

               PERFORM FETCH-MEMBER-NAME
               PERFORM FETCH-BOOK-NAME


               DISPLAY l_tran_id "    "
                       match_member_name(1:18)
                       match_book_name(1:30)
                       l_start_date " " " "
                       l_due_flag " "
                       l_return_date

               READ LogFile
                   AT END MOVE 'Y' TO EOF-Log
               END-READ
           END-PERFORM
           DISPLAY deco-line
           CLOSE LogFile MemberFile BookFile

           EXIT PROGRAM
           STOP RUN.

      *-----------------------------------------------------------------      *-----------------------
       FETCH-MEMBER-NAME.
           MOVE SPACES TO match_member_name
           MOVE 'N' TO EOF-Member

           CLOSE MemberFile
           OPEN INPUT MemberFile
           READ MemberFile
               AT END MOVE 'Y' TO EOF-Member
           END-READ

           PERFORM UNTIL EOF-Member = 'Y'
               OR match_member_name NOT = SPACES

               UNSTRING MemberRecord DELIMITED BY ","
                   INTO m_member_id, m_name, m_email,
                        m_address, m_gender, m_flag
               END-UNSTRING

               IF l_member_id = m_member_id
                   MOVE m_name TO match_member_name
               END-IF

               READ MemberFile
                   AT END MOVE 'Y' TO EOF-Member
               END-READ
           END-PERFORM

           IF match_member_name = SPACES
               MOVE "NOT FOUND" TO match_member_name
           END-IF.

      *-----------------------------------------------------------------      *-----------------------
       FETCH-BOOK-NAME.
           MOVE SPACES TO match_book_name
           MOVE 'N' TO EOF-Book

           CLOSE BookFile
           OPEN INPUT BookFile
           READ BookFile
               AT END MOVE 'Y' TO EOF-Book
           END-READ

           PERFORM UNTIL EOF-Book = 'Y' OR match_book_name NOT = SPACES
               UNSTRING BookRecord DELIMITED BY ","
                   INTO b_book_id, b_name, b_author,
                        b_count, b_genre
               END-UNSTRING

               IF l_book_id = b_book_id
                   MOVE b_name TO match_book_name
               END-IF

               READ BookFile
                   AT END MOVE 'Y' TO EOF-Book
               END-READ
           END-PERFORM

           IF match_book_name = SPACES
               MOVE "NOT FOUND" TO match_book_name
           END-IF.
      *-----------------------------------------------------------------
       END PROGRAM ShowHistoryLogs.
