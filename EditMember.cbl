      ******************************************************************
      * Author: Khant Ko
      * Date:   18.7.2025
      * Purpose:To update member information for a given ID
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EditMember.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MemberFile ASSIGN TO '../members.csv'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS file_status.
       DATA DIVISION.
       FILE SECTION.
       FD  MemberFile.
       01  member PIC X(200).
       WORKING-STORAGE SECTION.
       01  file_status PIC XX.
       01  found_flag PIC X VALUE 'N'.
       01  search_member_id PIC 9(5).
       01  quote_in_addr PIC 9 VALUE 0.
       01  total_member PIC 9(5).
       01  dummy PIC X.
       01  EOF PIC X VALUE 'N'.
       01  members PIC X(200) OCCURS 1000 TIMES INDEXED BY IDX.
       01  member_record .
           05  member_id         PIC 9(5).
           05  member_name       PIC X(30).
           05  member_email      PIC X(35).
           05  member_addr       PIC X(50).
           05  member_gender     PIC X.
           05  member_flag       PIC X(10).
           05 id_to_email        PIC X(70).
           05 gender_n_flag      PIC X(11).
       01  new_member_record .
           05  new_member_id         PIC 9(5).
           05  new_member_name       PIC X(30).
           05  new_member_email      PIC X(35).
           05  new_member_addr       PIC X(50).
           05  new_member_gender     PIC X.
           05  new_member_flag       PIC X(10).
       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.
           PERFORM MAIN-PROCEDURE
           EXIT PROGRAM.
           STOP RUN.
       MAIN-PROCEDURE.
            DISPLAY "Please, Enter Member ID To Update:"
            ACCEPT  search_member_id
            OPEN INPUT MemberFile.
            IF file_status not = '00' THEN
                DISPLAY "Error opening File, Status :"file_status
            END-IF
            MOVE 'N' TO EOF
            SET IDX TO 0
            PERFORM UNTIL EOF = 'Y'
               READ MemberFile
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   SET IDX UP BY 1
                   UNSTRING member DELIMITED BY "," INTO member_id
                   IF member_id = search_member_id THEN
                       MOVE 'Y' TO found_flag
                       PERFORM PROCESS-FOUND-MEMBER-DATA
                   ELSE
                       MOVE member TO members(IDX)
                   END-IF
               END-READ
            END-PERFORM
            MOVE IDX TO total_member
            CLOSE MemberFile
            IF found_flag = 'Y' THEN
                OPEN OUTPUT MemberFile
                IF file_status not = '00' THEN
                    DISPLAY "Error Opening File, Status:"file_status
                    EXIT PROGRAM
                END-IF
                SET IDX TO 1
                PERFORM UNTIL IDX > total_member
                   MOVE members(IDX) TO member
                   WRITE member
                   SET IDX UP BY 1
                END-PERFORM
                CLOSE MemberFile
                DISPLAY "-----------------------------------------"
                DISPLAY "Member ID:"new_member_id" Info Updated!"
            ELSE
                DISPLAY 'No Member Found!'
            END-IF.

           PROCESS-FOUND-MEMBER-DATA.
           INSPECT member TALLYING quote_in_addr FOR ALL '"'
           IF quote_in_addr > 0 THEN
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
           DISPLAY "Current Member Name: "member_name
           DISPLAY "Current Member Email: "member_email
           DISPLAY "Current Member Address: "member_addr
           DISPLAY "Current Member Gender: "member_gender
           DISPLAY "Current Member Flag: "member_flag
           DISPLAY "-----------------------------------------"
           MOVE member_id TO new_member_id
           DISPLAY "Enter New Name (or press ENTER to skip): "
           ACCEPT new_member_name
           IF new_member_name = SPACES THEN
               MOVE member_name TO new_member_name
           END-IF
           DISPLAY "Enter New Email (or press ENTER to skip): "
           ACCEPT new_member_email
           IF new_member_email = SPACES THEN
               MOVE member_email TO new_member_email
           END-IF
           DISPLAY "Enter New Address (or press ENTER to skip): "
           ACCEPT new_member_addr
           IF new_member_addr = SPACES THEN
               MOVE member_addr TO new_member_addr
           END-IF
           DISPLAY "Enter New Gender (or press ENTER to skip): "
           ACCEPT new_member_gender
           IF new_member_gender = SPACES THEN
               MOVE member_gender TO new_member_gender
           END-IF
           DISPLAY "Change Flag (or press ENTER to skip): "
           ACCEPT new_member_flag
           IF new_member_flag = SPACES THEN
               MOVE member_flag TO new_member_flag
           END-IF

           STRING
                   new_member_id        DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(new_member_name)    DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(new_member_email)   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   FUNCTION TRIM(new_member_addr) DELIMITED BY SIZE
                   '"'                   DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(new_member_gender)  DELIMITED BY SIZE
                   ","                   DELIMITED BY SIZE
                   FUNCTION TRIM(new_member_flag)  DELIMITED BY SIZE
                   INTO members(IDX)
           END-STRING.


       END PROGRAM EditMember.
