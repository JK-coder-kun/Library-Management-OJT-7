   ******************************************************************
      * Author: HL
      * Date: 7/8/2025
      * Purpose: Create New Member and store in CSV (Subprogram)
      * Notes: Fixes multiline issue with clean TRIM usage
      * Notes: Subprogram version with full input prompts and file I/O
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddNewMember.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MemberFile ASSIGN TO '../members.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT MemberFileIn ASSIGN TO '../members.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MemberFile.
       01  MemberRecord        PIC X(200).

       FD  MemberFileIn.
       01  MemberRecordIn      PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-CSV-LINE             PIC X(200).
       01  WS-HEADER               PIC X(200)
           VALUE "MemberID,Name,Email,Address,Gender,Status".
       01  EOF                     PIC X VALUE "N".
       01  cm_choice               PIC 9(1).

       01  member_record.
           05  member_id          PIC 9(5) VALUE 0.
           05  member_name        PIC X(30).
           05  member_email       PIC X(35).
           05  member_address     PIC X(50).
           05  member_gender      PIC X(1).
           05  member_status      PIC X(8) VALUE "ACTIVE".
       01  ws-valid-gender       PIC X VALUE "N".
       01  ws-trimmed-gender     PIC X(1).
       01  ws-valid-email         PIC X VALUE "N".
       01  ws-email-trimmed       PIC X(35).
       01  ws-at-count            PIC 9(2) VALUE 0.
       01  ws-dot-exist           PIC X VALUE "N".
       01  ws-i                   PIC 9(2) VALUE 1.
       01  ws-email-char          PIC X.
       01  ws-length              PIC 9(2).

       01  member_id_disp         PIC 9(5).
       01  last_line              PIC X(200).
       01  last_member_id_str     PIC X(5).
       01  last_member_id         PIC 9(5).

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PARA
           EXIT PROGRAM.
           STOP RUN.
       MAIN-PARA.

           *> Find last used member ID
           *>----------------------------------
           MOVE "N" TO EOF
           MOVE SPACES TO last_line

           OPEN INPUT MemberFileIn
           PERFORM UNTIL EOF = "Y"
               READ MemberFileIn
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       MOVE MemberRecordIn TO last_line
               END-READ
           END-PERFORM
           CLOSE MemberFileIn
           *>----------------------------------

           IF FUNCTION LENGTH(FUNCTION TRIM(last_line)) > 0 THEN
               UNSTRING last_line DELIMITED BY ","
                   INTO last_member_id_str
              MOVE FUNCTION NUMVAL(last_member_id_str) TO last_member_id
               ADD 1 TO last_member_id GIVING member_id
           ELSE
               MOVE 1 TO member_id
           END-IF

           MOVE member_id TO member_id_disp
           DISPLAY "Generated Member ID: " member_id_disp

           DISPLAY " "
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "* New Member Registration                     *"
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "* Enter Name       : "
           ACCEPT member_name


           PERFORM UNTIL ws-valid-email = "Y"
           DISPLAY "* Enter Email      : "
           ACCEPT member_email
           MOVE FUNCTION TRIM(member_email) TO ws-email-trimmed
           MOVE 0 TO ws-at-count
           MOVE "N" TO ws-dot-exist
           MOVE 1 TO ws-i
           MOVE FUNCTION LENGTH(ws-email-trimmed) TO ws-length

           PERFORM VARYING ws-i FROM 1 BY 1 UNTIL ws-i > ws-length
               MOVE ws-email-trimmed(ws-i:1) TO ws-email-char
               IF ws-email-char = "@"
                   ADD 1 TO ws-at-count
               ELSE IF ws-email-char = "."
                   MOVE "Y" TO ws-dot-exist
               END-IF
           END-PERFORM

           IF ws-at-count = 1 AND ws-dot-exist = "Y"
               MOVE ws-email-trimmed TO member_email
               MOVE "Y" TO ws-valid-email
           ELSE
               DISPLAY "!! Invalid email. Must contain '@' and '.'"
           END-IF
           END-PERFORM



           DISPLAY "* Enter Address    : "
           ACCEPT member_address


           PERFORM UNTIL ws-valid-gender = "Y"
               DISPLAY "* Enter Gender (M/F): "
               ACCEPT member_gender
               MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(member_gender))
               TO ws-trimmed-gender

               IF ws-trimmed-gender = "M" OR ws-trimmed-gender = "F"
                   MOVE ws-trimmed-gender TO member_gender
                   MOVE "Y" TO ws-valid-gender
               ELSE
                   DISPLAY "!! Invalid Gender. Enter only M or F."
               END-IF
           END-PERFORM



           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "Enter 1. to create, 0. to exit:  "
           ACCEPT cm_choice
           DISPLAY " "

           IF cm_choice = 1 THEN
      *>          STRING
      *>              member_id_disp        DELIMITED BY SIZE
      *>              ","                   DELIMITED BY SIZE
      *>              FUNCTION TRIM(member_name)    DELIMITED BY SIZE
      *>              ","                   DELIMITED BY SIZE
      *>              FUNCTION TRIM(member_email)   DELIMITED BY SIZE
      *>              ","                   DELIMITED BY SIZE
      *>              '"'                   DELIMITED BY SIZE
      *>              FUNCTION TRIM(member_address) DELIMITED BY SIZE
      *>              '"'                   DELIMITED BY SIZE
      *>              ","                   DELIMITED BY SIZE
      *>              FUNCTION TRIM(member_gender)  DELIMITED BY SIZE
      *>              ","                   DELIMITED BY SIZE
      *>              FUNCTION TRIM(member_status)  DELIMITED BY SIZE
      *>              INTO WS-CSV-LINE
      *>          END-STRING

      *>          OPEN EXTEND MemberFile

               IF last_member_id = 0 THEN
                   MOVE 10001 TO member_id_disp
                   OPEN OUTPUT MemberFile
               ELSE
                   OPEN EXTEND MemberFile
               END-IF

               STRING
                   member_id_disp        DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   member_name    DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                  member_email   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   member_address DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   member_gender  DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   member_status  DELIMITED BY SIZE
                   INTO WS-CSV-LINE
               END-STRING

               MOVE WS-CSV-LINE TO MemberRecord
               WRITE MemberRecord

               CLOSE MemberFile

               DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
               DISPLAY "=== Member created successfully. ==="
               DISPLAY "* Member Name  :  " FUNCTION TRIM(member_name)
               DISPLAY "* Member ID    :  " member_id_disp
               DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"

           ELSE
               DISPLAY "New Member is not created."
           END-IF.

           MOVE 'N' TO ws-valid-email.
           MOVE 'N' TO ws-dot-exist.
           MOVE 'N' TO ws-valid-gender.

           END PROGRAM AddNewMember.
      *>      STOP RUN.
