      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION.

      ***************************************************MAIN MENU START
     * MAIN-PROCEDURE.
     *      DISPLAY "=============================="
     *      DISPLAY         "LIBRARY SYSTEM"
     *      DISPLAY "=============================="
     *      DISPLAY "1) Members"
     *      DISPLAY "2) Books"
     *      DISPLAY "3) Operations"
     *      DISPLAY "4) Record"
     *      DISPLAY "5) Exit"
     *      DISPLAY "Enter your choice (1-5): "
     *      ACCEPT USER-CHOICE
     *
     *      EVALUATE USER-CHOICE
     *         WHEN 1
     *             PERFORM MEMBERS-MENU
     *         WHEN 2
     *             PERFORM BOOKS-MENU
     *         WHEN 3
     *             PERFORM OPERATIONS-MENU
     *         WHEN 4
     *             PERFORM RECORD-MENU
     *         WHEN 5
     *              DISPLAY "Exiting... Goodbye!"
     *         WHEN OTHER
     *              DISPLAY "Invalid choice. Please try again."
     *              GO TO MAIN-PROCEDURE
     *      END-EVALUATE.
     *      STOP RUN.
     ******************************************************MAIN MENU END
     * MEMBERS-MENU.
     *      DISPLAY "=============================="
     *      DISPLAY          "MEMBERS MENU"
     *      DISPLAY "=============================="
     *      DISPLAY "1) List All Members"
     *      DISPLAY "2) Member Details"
     *      DISPLAY "3) Add New Member"
     *      DISPLAY "4) Exit"
     *      DISPLAY "Enter your choice (1-4): "
     *      ACCEPT USER-CHOICE
     *
     *      EVALUATE USER-CHOICE
     *         WHEN 1
     *             PERFORM LIST-ALL-MEMBERS
     *         WHEN 2
     *             PERFORM MEMBER-DETAIL
     *         WHEN 3
     *             PERFORM ADD-NEW-MEMBER
     *         WHEN 4
     *             DISPLAY "Exiting to Main Menu..."
     *             GO TO MAIN-PROCEDURE
     *         WHEN OTHER
     *             DISPLAY "Invalid choice. Please try again."
     *             GO TO MEMBERS-MENU
     *      END-EVALUATE.
     *
     * BOOKS-MENU.
     *      DISPLAY "=============================="
     *      DISPLAY          "BOOKS MENU"
     *      DISPLAY "=============================="
     *      DISPLAY "1) List All Books"
     *      DISPLAY "2) Searck Books"
     *      DISPLAY "3) Add New Book"
     *      DISPLAY "4) Update Book Info"
     *      DISPLAY "5) Exit"
     *      DISPLAY "Enter your choice (1-5): "
     *      ACCEPT USER-CHOICE
     *
     *      EVALUATE USER-CHOICE
     *         WHEN 1
     *             PERFORM LIST-ALL-BOOKS
     *         WHEN 2
     *              PERFORM SEARCH-BOOKS
     *          WHEN 3
     *              PERFORM ADD-NEW-BOOK
     *          WHEN 4
     *              PERFORM UPDATE-BOOK-INFO
     *          WHEN 5
     *              DISPLAY "Exiting to Main Menu..."
                    GO TO MAIN-PROCEDURE
     *          WHEN OTHER
     *              DISPLAY "Invalid choice. Please try again."
     *              GO TO BOOKS-MENU
     *      END-EVALUATE.
     *
     * OPERATIONS-MENU.
     *      DISPLAY "=============================="
     *      DISPLAY        "OPERATIONS MENU"
     *      DISPLAY "=============================="
     *      DISPLAY "1) Borrow Book"
     *      DISPLAY "2) Return Book"
     *      DISPLAY "3) Exit"
     *      DISPLAY "Enter your choice (1-3): "
     *      ACCEPT USER-CHOICE
     *
     *      EVALUATE USER-CHOICE
     *         WHEN 1
     *             PERFORM BORROW-BOOK
     *         WHEN 2
     *             PERFORM RETURN-BOOK
     *         WHEN 3
     *             DISPLAY "Exiting to Main Menu..."
                   GO TO MAIN-PROCEDURE
     *         WHEN OTHER
     *             DISPLAY "Invalid choice. Please try again."
     *             GO TO OPERATIONS-MENU
     *      END-EVALUATE.
     *
     * RECORD-MENU.
     *      DISPLAY "=============================="
     *      DISPLAY          "RECORD MENU"
     *      DISPLAY "=============================="
     *      DISPLAY "1) Show History Log"
     *      DISPLAY "2) Show Borrowed books Log"
     *      DISPLAY "3) Show Fine Log"
     *      DISPLAY "4) Exit"
     *      DISPLAY "Enter your choice (1-4): "
     *      ACCEPT USER-CHOICE
     *
     *      EVALUATE USER-CHOICE
     *         WHEN 1
     *             PERFORM SHOW-HISTROY-LOG
     *         WHEN 2
     *             PERFORM SHOW-BORROWED-BOOKS-LOG
     *         WHEN 3
     *             PERFORM SHOW-FINE-LOG
     *         WHEN 4
     *             DISPLAY "Exiting to Main Menu..."
                   GO TO MAIN-PROCEDURE
     *         WHEN OTHER
     *             DISPLAY "Invalid choice. Please try again."
     *             GO TO RECORD-MENU
     *      END-EVALUATE.
      ******************************************************************
           LIST-ALL-MEMBERS.
           CALL 'ListAllMembers' USING USER-CHOICE
           GO TO MEMBERS-MENU.

           MEMBER-DETAIL.
           CALL 'MemberDetail' USING USER-CHOICE
           GO TO MEMBERS-MENU.

           ADD-NEW-MEMBER.
           CALL 'AddNewMember' USING USER-CHOICE
           GO TO MEMBERS-MENU.

           LIST-ALL-BOOKS.
           CALL 'ListAllBooks' USING USER-CHOICE
           GO TO BOOKS-MENU.

           SEARCH-BOOKS.
           CALL 'SearchBook' USING USER-CHOICE
           GO TO BOOKS-MENU.

           ADD-NEW-BOOK.
           CALL 'AddNewBook' USING USER-CHOICE
           GO TO BOOKS-MENU.

           UPDATE-BOOK-INFO.
           CALL 'UpdateBook' USING USER-CHOICE
           GO TO BOOKS-MENU.

           BORROW-BOOK.
           CALL 'BorrowBook' USING USER-CHOICE
           GO TO OPERATIONS-MENU.

           RETURN-BOOK.
           CALL 'ReturnBook' USING USER-CHOICE
           GO TO OPERATIONS-MENU.

           SHOW-HISTROY-LOG.
           CALL 'ShowHistoryLog' USING USER-CHOICE
           GO TO RECORD-MENU.

           SHOW-BORROWED-BOOKS-LOG.
           CALL 'ShowBorrowedBooks' USING USER-CHOICE
           GO TO RECORD-MENU.

           SHOW-FINE-LOG.
           CALL 'ShowFineLog' USING USER-CHOICE
           GO TO RECORD-MENU.

       END PROGRAM YOUR-PROGRAM-NAME.
