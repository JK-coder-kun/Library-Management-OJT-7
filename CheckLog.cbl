      *****************************************************************
      * Author: Ei Khine Moe
      * Date: 14/07/2025
      * Purpose: Only update due_flag if return_date is blank
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CheckLog.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "../log.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-LINE       PIC X(200).

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

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).
       PROCEDURE DIVISION USING USER-CHOICE.

      * Get system date and convert to integer
           ACCEPT SYS-DATE FROM DATE YYYYMMDD
           COMPUTE SYS-DATE-INT = FUNCTION INTEGER-OF-DATE(SYS-DATE)

      * Read all records from log.csv into table
           OPEN INPUT LOG-FILE
           PERFORM UNTIL FLG-EOF = 'Y'
               READ LOG-FILE
                   AT END
                       MOVE 'Y' TO FLG-EOF
                   NOT AT END
                       UNSTRING LOG-LINE DELIMITED BY ","
                           INTO F-ID(IDX-CNT)
                                F-MID(IDX-CNT)
                                F-BID(IDX-CNT)
                                F-SDT(IDX-CNT)
                                F-EDT(IDX-CNT)
                                F-DUE(IDX-CNT)
                                F-RDT(IDX-CNT)

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
                           ELSE
                               MOVE "NO " TO F-DUE(IDX-CNT)
                           END-IF
                       END-IF

                       ADD 1 TO CNT-LOG
                       ADD 1 TO IDX-CNT
               END-READ
           END-PERFORM
           CLOSE LOG-FILE

      * Rewrite log.csv with updated due_flag but original return_date
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

           DISPLAY "CheckLog completed. "
           GOBACK.

       END PROGRAM CheckLog.
