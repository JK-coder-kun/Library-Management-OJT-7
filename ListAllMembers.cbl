      ******************************************************************
      * Author: Khant Ko
      * Date: 10/7/2025
      * Purpose: To list All Member in Members.csv, Use Pagination in listing file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ListAllMembers.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MemberFile ASSIGN TO "../members.csv"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS file-status.

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       FILE SECTION.
      *-----------------------
       FD MemberFile.
       01 member PIC X(140).
       *> 01 member_record .
           *> 05  member_id         PIC 9(5).
           *> 05  fill_comma        PIC X.
           *> 05  member_name       PIC X(30).
           *> 05  fill_comma        PIC X.
           *> 05  member_email      PIC X(35).
           *> 05  fill_comma        PIC X.
           *> 05  member_addr       PIC X(50).
           *> 05  fill_comma        PIC X.
           *> 05  member_gender     PIC X.
           *> 05  fill_comma        PIC X.
           *> 05  member_flag       PIC X(10).
       WORKING-STORAGE SECTION.
      *-----------------------
       01  file-status PIC XX.
       01  choice      PIC X.
       01  counter PIC 999 value 0.
       01  comma_in_addr PIC 9.
       01  dummy PIC X.
       01  EOF PIC X VALUE 'N'.
       01  continue_flag     PIC X VALUE "Y".
       01 member_record .
           05  member_id         PIC 9(5).
           05  member_name       PIC X(30).
           05  member_email      PIC X(35).
           05  member_addr       PIC X(50).
           05  member_gender     PIC X.
           05  member_flag       PIC X(10).
           05 id_to_email        PIC X(70).
           05 gender_n_flag      PIC X(11).
       01  DISPLAY-ROW.
           05 D-ID        PIC X(5).
           05 FILLER      PIC X VALUE " ".
           05 FILLER      PIC X VALUE "|".
           05 D-NAME      PIC X(30).
           05 FILLER      PIC X(2) VALUE " |".
           05 D-EMAIL     PIC X(35).
           05 FILLER      PIC X(2) VALUE " |".
           05 D-ADDR      PIC X(40).
           05 FILLER      PIC X(2) VALUE " |".
           05 D-GENDER    PIC X.
           05 FILLER      PIC X(9) VALUE "      |".
           05 D-FLAG      PIC X(10).
       01 DISPLAY-HEADER.
           05 FILLER         PIC X(5)  VALUE " ID  ".
           05 FILLER         PIC X     VALUE " ".
           05 FILLER         PIC X     VALUE "|".
           05 FILLER         PIC X(30) VALUE " Name".
           05 FILLER         PIC X(2)  VALUE " |".
           05 FILLER         PIC X(35) VALUE " Email".
           05 FILLER         PIC X(2)  VALUE " |".
           05 FILLER         PIC X(40) VALUE " Address".
           05 FILLER         PIC X(2)  VALUE " |".
           05 FILLER         PIC X(1)  VALUE "G".
           05 FILLER         PIC X(6)  VALUE "ender ".
           05 FILLER         PIC X(2)  VALUE "| ".
           05 FILLER         PIC X(10) VALUE " Flag    ".
       01 DECOR-LINE PIC X(134) VALUE ALL "*-".
       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            OPEN INPUT MemberFile.
            IF file-status not = '00' THEN
                DISPLAY "Error opening File, Status :"file-status
                GO TO ENDER
            END-IF
            MOVE 'N' TO EOF
            DISPLAY DECOR-LINE"*"

            DISPLAY DISPLAY-HEADER
            DISPLAY DECOR-LINE "*"
            MOVE 0 TO counter
            PERFORM UNTIL EOF = 'Y'
               READ MemberFile
               AT END MOVE 'Y' TO EOF
               NOT AT END

                   INSPECT member TALLYING comma_in_addr FOR ALL '"'
                   IF comma_in_addr > 0 THEN
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


                   MOVE member_id     TO D-ID
                   MOVE member_name   TO D-NAME
                   MOVE member_email  TO D-EMAIL
                   MOVE member_addr   TO D-ADDR
                   MOVE member_gender TO D-GENDER
                   MOVE member_flag   TO D-FLAG

                   DISPLAY DISPLAY-ROW

                   ADD 1 TO counter
                   MOVE 0 TO comma_in_addr
                   IF counter >= 10 THEN
                       MOVE 0 TO counter
                       DISPLAY "Press Enter (To Show Next Page) or"
                       " Q(To Quit):"
                       ACCEPT choice
                       IF choice = "Q" OR choice = "q" THEN
                           MOVE 'Y' TO EOF
                       END-IF
                   END-IF
               END-READ
            END-PERFORM.
            DISPLAY DECOR-LINE"*"
            CLOSE MemberFile.
            *> STOP RUN.
      ** add other procedures here
       ENDER.
       END PROGRAM ListAllMembers.
