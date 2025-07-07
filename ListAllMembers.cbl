      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EmployeeFile ASSIGN TO "employee.rel"
       ORGANIZATION IS RELATIVE
       ACCESS MODE IS DYNAMIC
       RELATIVE KEY IS RelativeKey
       FILE STATUS IS FileStatus.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01  WS-CHOICE         PIC 9.
       01  WS-CSV-LINE       PIC X(100).
       *> -------------------------------------------------------
       01  WS-FileStatus     PIC XX.
       01 member_record .
           05  member_id         PIC 9(5).
           05  member_name       PIC X(30).
           05  member_email      PIC X(35).
           05  member_address    PIC X(50).
           05  member_gender     PIC X.
           05  memeber_flag      PIC X(10).
       01 counter PIC 999 value 0.
       01  continue_flag     PIC X VALUE "Y".
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            OPEN INPUT
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
