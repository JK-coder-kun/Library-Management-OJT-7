      ******************************************************************
      * Author: Khant Ko
      * Date:   14.7.2025
      * Purpose: To show detail info of a member and his/her borrow history
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MemberDetail.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MemberFile ASSIGN TO "../members.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS file_status.
       SELECT BookFile ASSIGN TO "../books.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS file_status.
       SELECT LogFile ASSIGN TO "../log.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS file_status.

       DATA DIVISION.
       FILE SECTION.
       FD MemberFile.
       01 member PIC X(150).

       FD BookFile.
       01 book PIC X(100).

       FD LogFile.
       01 log PIC X(100).
       WORKING-STORAGE SECTION.
       01 file_status PIC XX.
       01 search_member_id PIC 9(5).
       01 log_member_id PIC 9(5).
       01 found_log_counter PIC 999 VALUE 0.
       01 overdue_unreturn_books PIC 99 VALUE 0.
       01 disp_count PIC ZZ9.
       01 EOF PIC X value "N".
       01 found_flag PIC X VALUE "N".
       01 book_found_flag PIC X VALUE "N".
       01  dummy PIC X.
       01 temp_book_id PIC 9(5).
       01 comma_in_addr PIC 9.
       01 header_displayed PIC X VALUE "N".
       01 member_record .
           05  member_id         PIC 9(5).
           05  member_name       PIC X(30).
           05  member_email      PIC X(35).
           05  member_addr       PIC X(50).
           05  member_gender     PIC X.
           05  member_flag       PIC X(10).
           05 id_to_email        PIC X(70).
           05 gender_n_flag      PIC X(11).
       01 member_record_header.
              05 FILLER              PIC X(6)   VALUE "ID".
              05 FILLER              PIC X(31)  VALUE "NAME".
              05 FILLER              PIC X(36)  VALUE "EMAIL".
              05 FILLER              PIC X(51)  VALUE "ADDRESS".
              05 FILLER              PIC X(8)   VALUE "GENDER".
              05 FILLER              PIC X(10)  VALUE "FLAG".

       01 member_history.
           05 FILLER              PIC X(5)   VALUE SPACES.
           05 log_id PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 book_id PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 book_name PIC X(30).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 start_date PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 end_date PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 return_date PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 due_flag PIC X(10).
        01 member_history_header.
           05 FILLER              PIC X(5)   VALUE SPACES.
           05 FILLER              PIC X(5)   VALUE "LOGID".
           05 FILLER              PIC X(2)   VALUE SPACES.
           05 FILLER              PIC X(5)   VALUE "BID".
           05 FILLER              PIC X(2)   VALUE SPACES.
           05 FILLER              PIC X(30)  VALUE "BOOK NAME".
           05 FILLER              PIC X(2)   VALUE SPACES.
           05 FILLER              PIC X(10)  VALUE "START DATE".
           05 FILLER              PIC X(5)   VALUE SPACES.
           05 FILLER              PIC X(10)  VALUE "END DATE".
           05 FILLER              PIC X(5)   VALUE SPACES.
           05 FILLER              PIC X(11)  VALUE "RETURN DATE".
           05 FILLER              PIC X(4)   VALUE SPACES.
           05 FILLER              PIC X(10)  VALUE "DUE FLAG".
       01 member_decor_line         PIC X(140) VALUE ALL "*-".
       01 decor_line         PIC X(101) VALUE ALL "*-".
       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.
       MAIN-PROCEDURE.
            OPEN INPUT MemberFile
                DISPLAY "Enter Member ID to Search: "
                ACCEPT search_member_id
                MOVE 'N' TO EOF
                MOVE 'N' TO found_flag
                PERFORM UNTIL EOF="Y" or found_flag = 'Y'
                   READ MemberFile
                       AT END MOVE 'Y' TO EOF
                       NOT AT END
                       UNSTRING member delimited by ","
                       INTO member_id
                       IF member_id = search_member_id THEN
                       MOVE 'Y' TO found_flag
                       DISPLAY member_record_header
                       DISPLAY member_decor_line'*'
                       INSPECT member TALLYING comma_in_addr FOR ALL '"'
                       IF comma_in_addr > 1 THEN
                           UNSTRING member DELIMITED BY '"'
                           INTO id_to_email member_addr gender_n_flag
                           UNSTRING id_to_email DELIMITED BY ','
                           INTO member_id member_name member_email
                           UNSTRING gender_n_flag DELIMITED BY ','
                           INTO dummy member_gender member_flag
                       ELSE
                           UNSTRING member DELIMITED
                           BY ',' INTO member_id member_name
                           member_email member_addr
                           member_gender member_flag
                       END-IF
                       DISPLAY member_id " "member_name" "member_email
                      " "member_addr" "member_gender"       "member_flag
                       DISPLAY " "
                       END-IF
                    END-READ
                END-PERFORM
                IF found_flag = 'N' THEN
                    DISPLAY 'No Member found!'
                    GO TO ENDER
                ELSE
                    PERFORM EXTRACT-HISTORY
                END-IF
            CLOSE MemberFile.

      *>       STOP RUN.
            EXTRACT-HISTORY.
                OPEN INPUT LogFile
                IF file_status not = '00' THEN
                    DISPLAY 'Error opening file "log.csv",status:'
                    file_status
                    GO TO ENDER
                END-IF
                MOVE 0 TO found_log_counter
                MOVE 0 TO overdue_unreturn_books
                MOVE 'N' TO header_displayed
                PERFORM UNTIL EOF='Y'
                   READ LogFile
                       AT END MOVE 'Y' TO EOF
                       NOT AT END
                       UNSTRING log DELIMITED BY ',' INTO log_id,
                       log_member_id
                       IF log_member_id = search_member_id THEN
                           ADD 1 to found_log_counter
                           IF header_displayed = "N" THEN

                               DISPLAY " "
                               DISPLAY "     "NO ADVANCING
                               DISPLAY decor_line
                               DISPLAY member_history_header
                               DISPLAY "     "NO ADVANCING
                               DISPLAY decor_line
                               MOVE "Y" TO header_displayed
                           END-IF
                           UNSTRING log DELIMITED BY ','
                           INTO log_id, log_member_id,book_id
                           ,start_date,end_date,due_flag, return_date
                           IF FUNCTION TRIM(return_date)=SPACE AND
                               due_flag = 'YES' THEN
                               ADD 1 TO overdue_unreturn_books
                           END-IF
                           MOVE 'N' TO book_found_flag
                           OPEN INPUT BookFile
                               PERFORM UNTIL book_found_flag="Y"
                               READ BookFile
                               AT END MOVE "Y" TO book_found_flag
                               NOT AT END
                               UNSTRING book DELIMITED BY ","
                               INTO temp_book_id
                               IF book_id = temp_book_id THEN
                                   MOVE "Y" TO book_found_flag
                                   UNSTRING book DELIMITED BY ","
                                   INTO temp_book_id,book_name
                               END-IF
                               END-READ
                               END-PERFORM
                           CLOSE BookFile
                           DISPLAY member_history
                       END-IF
                   END-READ
               END-PERFORM
               IF found_log_counter = 0 THEN
                   DISPLAY "No History Found!"
               ELSE
                   DISPLAY "     "decor_line
                   DISPLAY " "
                   MOVE found_log_counter TO disp_count
                   DISPLAY "Total History Made By Member: "
                   disp_count
                   MOVE overdue_unreturn_books TO disp_count
                   DISPLAY "Total Unreturned Books That are Overdue: "
                   disp_count
               END-IF
               CLOSE LogFile.
       ENDER.
       END PROGRAM MemberDetail.
