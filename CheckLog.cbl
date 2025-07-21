      ******************************************************************
      * Author: Ei Khine Moe
      * Date: 14/07/2025
      * Purpose: Update due_flag only if return_date is blank
      *          and set member_flag to INACTIVE
      *          if book is overdue and not returned
      * Addition( Khant Ko) : Check member ACTIVE Flag and Fixed address not
      *            Display correctly error
      * Addition( Htay Lwin) : Showing Summary for counts of updated active/inactive members
      *            and overdue books counts
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CheckLog.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MEMBER-FILE ASSIGN TO "../members.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-LINE       PIC X(200).

       FD  MEMBER-FILE.
       01  MEMBER-LINE    PIC X(200).

       WORKING-STORAGE SECTION.
       01  FLG-EOF        PIC X VALUE 'N'.
       01  IDX-CNT        PIC 9(3) VALUE 1.
       01  CNT-LOG        PIC 9(3) VALUE 0.

       01  SYS-DATE       PIC 9(8).
       01  SYS-DATE-INT   PIC 9(8).

       01  RAW-END-DT     PIC X(10).
       01  DT-DAY         PIC X(2).
       01  DT-MON         PIC X(2).
       01  DT-YEAR        PIC X(4).
       01  DT-FMT         PIC 9(8).
       01  DT-INT         PIC 9(8).

       01  LOG-DATA.
           05 LOG-ROW OCCURS 200 TIMES.
              10 F-ID     PIC X(5).
              10 F-MID    PIC X(5).
              10 F-BID    PIC X(5).
              10 F-SDT    PIC X(10).
              10 F-EDT    PIC X(10).
              10 F-DUE    PIC X(3).
              10 F-RDT    PIC X(10).
       01  MEMBER-DATA.
           05 MEM-ROW OCCURS 200 TIMES.
              10 M-ID       PIC X(5).
              10 M-NAME     PIC X(30).
              10 M-EMAIL    PIC X(35).
              10 M-ADDRESS  PIC X(50).
              10 M-GENDER   PIC X(1).
              10 M-FLAG     PIC X(8).
              10 M-UNRT-OVCT PIC 99 VALUE 0.

       01  M-IDX     PIC 9(3) VALUE 1.
       01  M-COUNT   PIC 9(3) VALUE 0.
       01  TMP-MID   PIC X(5).

      * New vars for address handling
       01  Q-CNT       PIC 9.
       01  id_to_email        PIC X(70).
       01  gender_n_flag      PIC X(11).
       01  dummy PIC X.

       01 CNT-OVERDUE   PIC 9(4) VALUE 0.
       01 CNT-INACTIVE  PIC 9(4) VALUE 0.


       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).

       PROCEDURE DIVISION USING USER-CHOICE.

      * Get system date and convert to integer
           ACCEPT SYS-DATE FROM DATE YYYYMMDD
           COMPUTE SYS-DATE-INT = FUNCTION INTEGER-OF-DATE(SYS-DATE)

      * Read all member records into memory
           MOVE 'N' TO FLG-EOF
           MOVE 1 TO M-IDX
           OPEN INPUT MEMBER-FILE
           PERFORM UNTIL FLG-EOF = 'Y'
               READ MEMBER-FILE
                   AT END
                       MOVE 'Y' TO FLG-EOF
                   NOT AT END
                       MOVE 0 TO Q-CNT
                       INSPECT MEMBER-LINE TALLYING Q-CNT FOR ALL '"'
                       IF Q-CNT > 0 THEN
                           UNSTRING MEMBER-LINE DELIMITED BY '"'
                           INTO id_to_email, M-ADDRESS(M-IDX),
                                gender_n_flag
                           UNSTRING id_to_email DELIMITED BY ','
                           INTO M-ID(M-IDX),M-NAME(M-IDX),M-EMAIL(M-IDX)
                           UNSTRING gender_n_flag DELIMITED BY ','
                           INTO dummy, M-GENDER(M-IDX), M-FLAG(M-IDX)
                       ELSE
                           UNSTRING MEMBER-LINE DELIMITED BY ','
                           INTO M-ID(M-IDX), M-NAME(M-IDX),
                                M-EMAIL(M-IDX), M-ADDRESS(M-IDX),
                                M-GENDER(M-IDX), M-FLAG(M-IDX)
                       END-IF

                       ADD 1 TO M-IDX
                       ADD 1 TO M-COUNT
               END-READ
           END-PERFORM
           CLOSE MEMBER-FILE

      * Read all records from log.csv into table
           MOVE 'N' TO FLG-EOF
           OPEN INPUT LOG-FILE
           PERFORM UNTIL FLG-EOF = 'Y'
               READ LOG-FILE
                   AT END
                       MOVE 'Y' TO FLG-EOF
                   NOT AT END
                       UNSTRING LOG-LINE DELIMITED BY ","
                           INTO F-ID(IDX-CNT), F-MID(IDX-CNT),
                           F-BID(IDX-CNT), F-SDT(IDX-CNT),
                           F-EDT(IDX-CNT), F-DUE(IDX-CNT),F-RDT(IDX-CNT)

                       IF FUNCTION TRIM(F-RDT(IDX-CNT)) = SPACE
                           MOVE F-EDT(IDX-CNT) TO RAW-END-DT
                           UNSTRING RAW-END-DT DELIMITED BY "-"
                               INTO DT-DAY, DT-MON, DT-YEAR
                           STRING DT-YEAR DELIMITED BY SIZE
                                  DT-MON  DELIMITED BY SIZE
                                  DT-DAY  DELIMITED BY SIZE
                               INTO DT-FMT
                       COMPUTE DT-INT = FUNCTION INTEGER-OF-DATE(DT-FMT)

                           IF SYS-DATE-INT > DT-INT
                               MOVE "YES" TO F-DUE(IDX-CNT)
                               ADD 1 TO CNT-OVERDUE
                           ELSE
                               MOVE "NO " TO F-DUE(IDX-CNT)
                           END-IF

                           IF F-DUE(IDX-CNT) = "YES"
                              PERFORM VARYING M-IDX FROM 1 BY 1
                                UNTIL M-IDX > M-COUNT
      *>                             IF F-MID(IDX-CNT) = M-ID(M-IDX)
      *>                                MOVE "INACTIVE" TO M-FLAG(M-IDX)
      *>                                ADD 1 TO M-UNRT-OVCT(M-IDX)

                                     IF F-MID(IDX-CNT) = M-ID(M-IDX)
                                       IF M-FLAG(M-IDX) NOT = "INACTIVE"
                                        MOVE "INACTIVE" TO M-FLAG(M-IDX)
                                        ADD 1 TO CNT-INACTIVE
                                     END-IF
                                        ADD 1 TO M-UNRT-OVCT(M-IDX)

                                  END-IF
                              END-PERFORM
                           END-IF
                       END-IF

                       ADD 1 TO CNT-LOG
                       ADD 1 TO IDX-CNT
               END-READ
           END-PERFORM
           CLOSE LOG-FILE

      * Rewrite log.csv with updated due_flag
           OPEN OUTPUT LOG-FILE
           PERFORM VARYING IDX-CNT FROM 1 BY 1 UNTIL IDX-CNT > CNT-LOG
               STRING
                   F-ID(IDX-CNT) DELIMITED BY SIZE ","
                   F-MID(IDX-CNT) DELIMITED BY SIZE ","
                   F-BID(IDX-CNT) DELIMITED BY SIZE ","
                   F-SDT(IDX-CNT) DELIMITED BY SIZE ","
                   F-EDT(IDX-CNT) DELIMITED BY SIZE ","
                   F-DUE(IDX-CNT) DELIMITED BY SIZE ","
                   F-RDT(IDX-CNT) DELIMITED BY SIZE
                   INTO LOG-LINE
               WRITE LOG-LINE
           END-PERFORM
           CLOSE LOG-FILE

      * Rewrite members.csv with updated flags
           OPEN OUTPUT MEMBER-FILE
           PERFORM VARYING M-IDX FROM 1 BY 1
             UNTIL M-IDX > M-COUNT
               IF M-UNRT-OVCT(M-IDX) = 0 THEN
                   MOVE 'ACTIVE' TO M-FLAG(M-IDX)
               END-IF
               STRING
                   M-ID(M-IDX) DELIMITED BY SIZE ","
                   M-NAME(M-IDX) DELIMITED BY SIZE ","
                   M-EMAIL(M-IDX) DELIMITED BY SIZE ","
                   '"' DELIMITED BY SIZE
                   M-ADDRESS(M-IDX) DELIMITED BY SIZE
                   '"' DELIMITED BY SIZE ","
                   M-GENDER(M-IDX) DELIMITED BY SIZE ","
                   M-FLAG(M-IDX) DELIMITED BY SIZE
                   INTO MEMBER-LINE
               WRITE MEMBER-LINE
           END-PERFORM
           CLOSE MEMBER-FILE

      *>      DISPLAY CNT-OVERDUE " overdue books found, "
      *>      DISPLAY CNT-INACTIVE " members marked inactive."

           GOBACK.
       END PROGRAM CheckLog.
