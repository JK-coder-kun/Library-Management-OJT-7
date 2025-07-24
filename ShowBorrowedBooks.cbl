      ******************************************************************
      * Author: Khant Ko
      * Date:   15.7.2025
      * Purpose:To show the book that are borrowed which are not returned by users
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ShowBorrowedBooks.

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
       01 EOF PIC X VALUE 'N'.

       01 member_src_id PIC 9(5).
       01 book_src_id PIC 9(5).
       01 no_book_flag PIC X VALUE "N".
       01 no_member_flag PIC X VALUE "N".
       01 total_not_return PIC 999 VALUE 0.
       01 total_due PIC 999 VALUE 0.
       01 disp_count PIC ZZ9.
       01 non_space_count PIC 99.

       01 history.
           05 FILLER PIC X(5)   VALUE SPACES.
           05 log_id PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 member_id PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 book_id PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 start_date PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 end_date PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 due_flag PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 return_date PIC X(10) VALUE "NONE".

       01 non_return_books OCCURS 1000 TIMES INDEXED BY IDX, MAX-IDX.
           05 logId PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 bookId PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 bookName PIC X(30) VALUE SPACES.
           05 FILLER PIC X(2)  VALUE SPACES.
           05 memberId PIC 9(5).
           05 FILLER PIC X(2)  VALUE SPACES.
           05 memberName PIC X(30) VALUE SPACES.
           05 FILLER PIC X(2)  VALUE SPACES.
           05 startDate PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 endDate PIC X(10).
           05 FILLER PIC X(5)  VALUE SPACES.
           05 dueFlag PIC X(10).


       01 NON-RETURN-BOOKS-HEADER.
           05 HDR-LOG-ID       PIC X(5)  VALUE "LOGID".
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 HDR-BOOK-ID      PIC X(5)  VALUE "BKID".
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 HDR-BOOK-NAME    PIC X(30) VALUE "BOOK NAME".
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 HDR-MEMBER-ID    PIC X(5)  VALUE "MBID".
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 HDR-MEMBER-NAME  PIC X(30) VALUE "MEMBER NAME".
           05 FILLER           PIC X(2)  VALUE SPACES.
           05 HDR-START-DATE   PIC X(10) VALUE "START DATE".
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 HDR-END-DATE     PIC X(10) VALUE "END DATE".
           05 FILLER           PIC X(5)  VALUE SPACES.
           05 HDR-DUE-FLAG     PIC X(10) VALUE "DUE FLAG".

       01 DECOR-LINE PIC X(125) VALUE ALL "*-".
       01 BEOF PIC X VALUE 'N'.
       01 MEOF PIC X VALUE 'N'.
       01  choice      PIC X.
       01  counter PIC 999 value 0.
       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.

       MAIN-PROCEDURE.
            SET IDX TO 1
            SET IDX DOWN BY 1
            MOVE 'N' TO EOF
            MOVE 0 TO total_not_return
            MOVE 0 TO total_due
            OPEN INPUT LogFile
            IF file_status not = '00' THEN
                DISPLAY "Error Opening File, Status: "file_status
                EXIT PROGRAM
            END-IF
            PERFORM UNTIL EOF = 'Y'
               MOVE 0 TO non_space_count
               READ LogFile
                   AT END
                       MOVE 'Y' TO EOF
                       SET MAX-IDX TO IDX
                   NOT AT END
                   UNSTRING log DELIMITED BY ','
                   INTO log_id, member_id, book_id, start_date
                   ,end_date, due_flag, return_date

                   INSPECT return_date TALLYING
                   non_space_count FOR CHARACTERS BEFORE INITIAL SPACE

                       IF non_space_count = 0 THEN
                           ADD 1 TO total_not_return
                           IF due_flag = "YES" THEN
                               ADD 1 TO total_due
                           END-IF
                           SET IDX UP BY 1
                           MOVE log_id to logId(IDX)
                           MOVE member_id to memberId(IDX)
                           MOVE book_id to bookId(IDX)
                           MOVE start_date to startDate(IDX)
                           MOVE end_date to endDate(IDX)
                           MOVE due_flag to dueFlag(IDX)

                           PERFORM EXTRACT-MEMBER-NAME
                           PERFORM EXTRACT-BOOK-NAME
                       END-IF
                END-READ
            END-PERFORM

            SORT non_return_books DESCENDING bookId
            IF IDX > 0 THEN
                DISPLAY " "
                DISPLAY "Currently Borrowed Books"
                DISPLAY "========================"
                DISPLAY " "

                DISPLAY DECOR-LINE
                DISPLAY NON-RETURN-BOOKS-HEADER
                DISPLAY DECOR-LINE
            END-IF
            MOVE 0 TO counter
            PERFORM UNTIL IDX = 0
               DISPLAY non_return_books(IDX)
               SET IDX DOWN BY 1
               ADD 1 TO counter
                    IF counter >= 10 THEN
                          MOVE 0 TO counter
                          DISPLAY "Press Enter (To Show Next Page)"
                               " or Q(To Quit):"
                          ACCEPT choice
                          IF choice = "Q" OR choice = "q" THEN
                               MOVE 0 TO IDX
                          END-IF
                    END-IF
            END-PERFORM

            DISPLAY " "
            MOVE total_not_return TO disp_count
            DISPLAY "Number of Books that are not returned:"
            disp_count
            MOVE total_due TO disp_count
            DISPLAY "Number of not returned Books that are over due:"
            disp_count
            CLOSE LogFile.

      *-----------------------------------------------------------------
       EXTRACT-MEMBER-NAME.
           OPEN INPUT MemberFile
               IF file_status not = '00' THEN
                    DISPLAY 'Error Opening file, '
                    'Status:'file_status
                    EXIT PROGRAM
               END-IF
               MOVE 'N' TO MEOF
               MOVE 'N' TO no_member_flag
               PERFORM UNTIL memberName(IDX) not = SPACES OR
                   MEOF = 'Y'
               READ MemberFile
               AT END
                   MOVE 'Y' TO MEOF
               NOT AT END
                   UNSTRING member DELIMITED BY ','
                   INTO member_src_id
                   IF member_src_id=member_id THEN
                       UNSTRING member DELIMITED BY ','
                       INTO member_src_id, memberName(IDX)
                   END-IF
               END-READ
               END-PERFORM
      *>          IF no_member_flag = 'Y' THEN
      *>              MOVE 'Not Found!' TO memberName(IDX)
      *>          END-IF
           CLOSE MemberFile.

      *-----------------------------------------------------------------
       EXTRACT-BOOK-NAME.
           OPEN INPUT BookFile
               IF file_status not = '00' THEN
                    DISPLAY 'Error Opening file, '
                    'Status:'file_status
                    EXIT PROGRAM
               END-IF
               MOVE 'N' TO BEOF
               MOVE 'N' TO no_book_flag
               PERFORM UNTIL bookName(IDX) not = SPACES OR
                   BEOF = 'Y'
               READ BookFile
               AT END
                    MOVE 'Y' TO BEOF
               NOT AT END
                   UNSTRING book DELIMITED BY ','
                   INTO book_src_id
                   IF book_src_id=book_id THEN
                       UNSTRING book DELIMITED BY ','
                       INTO book_src_id, bookName(IDX)
                   END-IF
               END-READ
               END-PERFORM
      *>          IF no_book_flag = 'Y' THEN
      *>              MOVE 'Not Found!' TO bookName(IDX)
      *>          END-IF
           CLOSE BookFile.
      *-----------------------------------------------------------------
       END PROGRAM ShowBorrowedBooks.
