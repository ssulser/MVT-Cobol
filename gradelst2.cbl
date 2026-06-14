       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-LIST2.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRADE-FILE ASSIGN TO UT-S-INPUT.
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  GRADE-FILE
           LABEL RECORD IS OMITTED.
       01  GRADE-RECORD.
           05  STUDENT-ID-IN           PIC X(10).
           05  STUDENT-NAME-IN         PIC X(25).
           05  FILLER                  PIC X(05).
           05  STUDENT-SCORE1-IN       PIC 99.
           05  FILLER                  PIC X(03).
           05  STUDENT-SCORE2-IN       PIC 99.
           05  FILLER                  PIC X(03).
           05  STUDENT-GRADE-IN        PIC X.
           05  FILLER                  PIC X(29).
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED.
       01  PRINT-LINE                  PIC X(132).
      *
       WORKING-STORAGE SECTION.
       77  TOTAL-CARDS                 PIC 99      VALUE ZERO.
       77  TOTAL-SCORE1                PIC 9(4)    VALUE ZERO.
       77  TOTAL-SCORE2                PIC 9(4)    VALUE ZERO.
       77  IS-EOF                      PIC X       VALUE 'N'.
      *
       01  HEADER-LINE.
           03  FILLER                  PIC X(43)   VALUE SPACES.
           03  FILLER                  PIC X(21)   VALUE
               'STUDENT GRADE SUMMARY'.
      *
       01  TITLE-LINE.
           03  FILLER                  PIC X(20)   VALUE SPACES.
           03  FILLER                  PIC X(12)   VALUE
               'STUDENT NAME'.
           03  FILLER                  PIC X(18)   VALUE SPACES.
           03  FILLER                  PIC X(09)   VALUE
               'ID NUMBER'.
           03  FILLER                  PIC X(12)   VALUE SPACES.
           03  FILLER                  PIC X(06)   VALUE
               'SCORES'.
           03  FILLER                  PIC X(04)   VALUE SPACES.
           03  FILLER                  PIC X(05)   VALUE
               'TOTAL'.
      *
       01  SUBTITLE-LINE.
           03  FILLER                  PIC X(71)   VALUE SPACES.
           03  FILLER                  PIC X       VALUE '1'.
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  FILLER                  PIC X       VALUE '2'.
           03  FILLER                  PIC X(03)   VALUE SPACES.
           03  FILLER                  PIC X(05)   VALUE 'SCORE'.
           03  FILLER                  PIC X(07)   VALUE '  GRADE'.
      *
       01  DETAIL-LINE.
           03  FILLER                  PIC X(20)   VALUE SPACES.
           03  DETAIL-NAME             PIC X(25).
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-ID               PIC X(10).
           03  FILLER                  PIC X(10)   VALUE SPACES.
           03  DETAIL-SCORE-1          PIC 99.
           03  FILLER                  PIC X(04)   VALUE SPACES.
           03  DETAIL-SCORE-2          PIC 99.
           03  FILLER                  PIC X(04)   VALUE SPACES.
           03  DETAIL-TOTAL-SCORE      PIC Z99.
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-GRADE            PIC X.
      *
       01  SUMMARY-LINE.
           03  FILLER                  PIC X(35)   VALUE SPACES.
           03  FILLER                  PIC X(20)   VALUE
               'AVERAGE SCRORES FOR '.
           03  SUMMARY-CARDS-READ      PIC 99      VALUE ZERO.
           03  FILLER                  PIC X(13)   VALUE
               ' STUDENTS:   '.
           03  SUMMARY-SCORE-1         PIC 99      VALUE ZERO.
           03  FILLER                  PIC X(04)   VALUE SPACES.
           03  SUMMARY-SCORE-2         PIC 99      VALUE ZERO.
           03  FILLER                  PIC X(04)   VALUE SPACES.
           03  SUMMARY-TOTAL-SCORE     PIC Z99     VALUE ZERO.
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION THRU INITIALIZATION-EXIT.
           PERFORM READ-AND-PRINT THRU READ-AND-PRINT-EXIT
               UNTIL IS-EOF = 'Y' OR TOTAL-CARDS = 99.
           PERFORM PRINT-SUMMARY.
           PERFORM CLOSING.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT GRADE-FILE.
           OPEN OUTPUT PRINT-FILE.
           WRITE PRINT-LINE FROM HEADER-LINE BEFORE 2 LINES.
           WRITE PRINT-LINE FROM TITLE-LINE BEFORE 2 LINES.
           WRITE PRINT-LINE FROM SUBTITLE-LINE AFTER 1 LINES.
           READ GRADE-FILE
               AT END MOVE 'Y' TO IS-EOF.
       INITIALIZATION-EXIT.
           EXIT.
      *
       READ-AND-PRINT.
           MOVE STUDENT-NAME-IN TO DETAIL-NAME.
           MOVE STUDENT-ID-IN TO DETAIL-ID.
           MOVE STUDENT-SCORE1-IN TO DETAIL-SCORE-1.
           MOVE STUDENT-SCORE2-IN TO DETAIL-SCORE-2.
           ADD  STUDENT-SCORE1-IN, STUDENT-SCORE2-IN
               GIVING DETAIL-TOTAL-SCORE.
           MOVE STUDENT-GRADE-IN TO DETAIL-GRADE.
           ADD 1 TO TOTAL-CARDS.
           ADD STUDENT-SCORE1-IN TO TOTAL-SCORE1.
           ADD STUDENT-SCORE2-IN TO TOTAL-SCORE2.
           WRITE PRINT-LINE FROM DETAIL-LINE AFTER 1 LINES.
           READ GRADE-FILE
               AT END MOVE 'Y' TO IS-EOF.
       READ-AND-PRINT-EXIT.
           EXIT.
      *
       PRINT-SUMMARY.
           MOVE TOTAL-CARDS TO SUMMARY-CARDS-READ.
           IF TOTAL-CARDS > 0 THEN
               DIVIDE TOTAL-CARDS INTO TOTAL-SCORE1
                   GIVING SUMMARY-SCORE-1 ROUNDED
               DIVIDE TOTAL-CARDS INTO TOTAL-SCORE2
                   GIVING SUMMARY-SCORE-2 ROUNDED.
      *    END-IF.
           ADD SUMMARY-SCORE-1, SUMMARY-SCORE-2
               GIVING SUMMARY-TOTAL-SCORE.
           WRITE PRINT-LINE FROM SUMMARY-LINE AFTER 2 LINES.
      *
       CLOSING.
           CLOSE GRADE-FILE.
           CLOSE PRINT-FILE.
/*
//GO.INPUT DD *
447-221   JOSHUA WATSON                 48   68   A
123-456   LAUREN RODRIGUEZ              27   33   B
654-852   NATHAN SCOTT                  63   55   C
159-753   BENJAMIN PEREZ                73   87   D
612-782   ASHLEY WARD                   60   65   E
963-852   EVELYN BELL                   18   14   F
854-698   GABRIEL ANDERSON              25   33   A
654-852   TIMOTHY CARTER                23   28   B
112-254   OLIVER MOORE                  31   44   C
787-584   OWEN WOOD                     55   65   D
446-187   ROBERT ROSS                   42   52   E
268-471   HEATHER THOMAS                38   58   F
784-885   MEGAN SANDERS                 55   60   A
456-753   AUBREY COLLINS                75   88   B
574-698   JAMES WASHINGTON              47   63   C
456-587   SIMON SULSER                  48   48   D
875-965   JAMES BROWN                   66   66   E
212-236   KEVIN BLUE                    52   87   F
985-654   HUGH GRANT                    95   55   A
265-121   JAMES KING                    25   43   B
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//
