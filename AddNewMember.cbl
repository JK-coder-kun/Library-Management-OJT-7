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
           05  member_name        PIC X(20).
           05  member_email       PIC X(30).
           05  member_address     PIC X(90).
           05  member_gender      PIC X(1).
           05  member_status      PIC X(10) VALUE "ACTIVE".

       01  member_id_disp         PIC 9(5).
       01  last_line              PIC X(200).
       01  last_member_id_str     PIC X(5).
       01  last_member_id         PIC 9(5).

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PARA
           EXIT PROGRAM.
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
           DISPLAY "* Enter Email      : "
           ACCEPT member_email
           DISPLAY "* Enter Address    : "
           ACCEPT member_address
           DISPLAY "* Enter Gender(M/F): "
           ACCEPT member_gender
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "Enter 1. to create, 0. to exit:  "
           ACCEPT cm_choice
           DISPLAY " "

           IF cm_choice = 1 THEN
               STRING
                   member_id_disp        DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(member_name)    DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(member_email)   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   FUNCTION TRIM(member_address) DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(member_gender)  DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(member_status)  DELIMITED BY SIZE
                   INTO WS-CSV-LINE
               END-STRING

               OPEN EXTEND MemberFile

               IF last_member_id = 0 THEN
                   MOVE WS-HEADER TO MemberRecord
                   WRITE MemberRecord
               END-IF

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
           END PROGRAM AddNewMember.
      *>      STOP RUN.
