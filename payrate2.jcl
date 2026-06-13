//COBOL JOB 1,'COBOL PROGRAM',CLASS=A,MSGCLASS=X
//COBOL  EXEC PROC=COBUCG
//COB.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYRATE2.
       AUTHOR. SIMON SULSER.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAY-FILE ASSIGN TO UT-S-INPUT.
           SELECT PRINT-FILE ASSIGN TO UT-S-OUTPUT.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PAY-FILE
           LABEL RECORD IS OMITTED.
       01  PAY-RECORD.
           05  EMP-NAME                PIC X(25).
           05  EMP-ID                  PIC X(10).
           05  EMP-DEPARTEMENT         PIC X(15).
           05  EMP-PAY-RATE            PIC 99V99.
           05  FILLER                  PIC X(26).
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED.
       01  PRINT-LINE                  PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       77  TOTAL-PAY-RATE              PIC 9(4)V99 VALUE 0.
       77  TOTAL-SALARY                PIC 9(6)V99 VALUE 0.
       77  AVERAGE-PAY-RATE            PIC 9(4)V99 VALUE 0.
       77  AVERAGE-SALARY              PIC 9(6)V99 VALUE 0.
       77  ACTUAL-SALARY               PIC 9(4)V99 VALUE 0.
       77  TOTAL-CARDS-READ            PIC 99      VALUE 0.
       77  IS-EOF                      PIC X       VALUE 'N'.
      *
       01  HEADER-LINE.
           03  FILLER                  PIC X(41)   VALUE SPACES.
           03  FILLER                  PIC X(27)   VALUE 
               'P A Y  R A T E  R E P O R T'.
      *
       01  TITLE-LINE.
           03  FILLER                  PIC X(20)   VALUE SPACES.
           03  FILLER                  PIC X(13)   VALUE 
               'EMPLOYEE NAME'.
           03  FILLER                  PIC X(17)   VALUE SPACES.
           03  FILLER                  PIC X(09)   VALUE
               'ID NUMBER'.
           03  FILLER                  PIC X(06)   VALUE SPACES. 
           03  FILLER                  PIC X(10)   VALUE
               'DEPARTMENT'.
           03  FILLER                  PIC X(10)   VALUE SPACES.  
           03  FILLER                  PIC X(08)   VALUE
               'PAY RATE'.
           03  FILLER                  PIC X(03)   VALUE SPACES.  
           03  FILLER                  PIC X(06)   VALUE
               'SALARY'.
      *
       01  DETAIL-LINE.
           03  FILLER                  PIC X(20)   VALUE SPACES.
           03  DETAIL-NAME             PIC X(25).  
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-ID               PIC X(10).  
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-DEPARTMENT       PIC X(15).  
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-PAY-RATE         PIC $$9.99. 
           03  FILLER                  PIC X(05)   VALUE SPACES.
           03  DETAIL-SALARY           PIC $$$$9.99.
      *
       01  SUMMARY-LINE.
           03  FILLER                  PIC X(55)   VALUE SPACES.
           03  FILLER                  PIC X(12)   VALUE
               'AVERAGE FOR '.
           03  SUMMARY-TOTAL-CARDS     PIC 99.
           03  FILLER                  PIC X(14)   VALUE
               ' EMPLOYEES    '.
           03  SUMMARY-PAY-RATE        PIC $$$$9.99.
           03  FILLER                  PIC X(03)   VALUE SPACES.
           03  SUMMARY-SALARY          PIC $$$$$$9.99.
      *
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION THRU INITIALIZATION-EXIT.
           PERFORM READ-AND-PRINT THRU READ-AND-PRINT-EXIT
               UNTIL IS-EOF = 'Y' OR TOTAL-CARDS-READ = 99.
           PERFORM PRINT-TOTALS-AND-CLOSE.
           PERFORM CLOSING.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT PAY-FILE.
           OPEN OUTPUT PRINT-FILE.
           WRITE PRINT-LINE FROM HEADER-LINE BEFORE 2 LINES.
           WRITE PRINT-LINE FROM TITLE-LINE BEFORE 2 LINES.
           READ PAY-FILE
               AT END MOVE 'Y' TO IS-EOF.
       INITIALIZATION-EXIT.
           EXIT.
      *
       READ-AND-PRINT.
           MOVE EMP-NAME TO DETAIL-NAME.
           MOVE EMP-ID TO DETAIL-ID.
           MOVE EMP-DEPARTEMENT TO DETAIL-DEPARTMENT.
           MOVE EMP-PAY-RATE TO DETAIL-PAY-RATE.
      * CALCULATING TOTAL SALARY FOR 40H WEEK
           ADD  EMP-PAY-RATE TO TOTAL-PAY-RATE.
           MULTIPLY EMP-PAY-RATE BY 40 GIVING ACTUAL-SALARY.
           MOVE ACTUAL-SALARY TO DETAIL-SALARY.
           ADD ACTUAL-SALARY TO TOTAL-SALARY.
           ADD 1 TO TOTAL-CARDS-READ.
           WRITE PRINT-LINE FROM DETAIL-LINE AFTER 1 LINES.
           READ PAY-FILE
               AT END MOVE 'Y' TO IS-EOF.
       READ-AND-PRINT-EXIT.
           EXIT.
      *
       PRINT-TOTALS-AND-CLOSE.
           IF TOTAL-CARDS-READ > 0 THEN
               DIVIDE TOTAL-CARDS-READ INTO TOTAL-PAY-RATE
                   GIVING AVERAGE-PAY-RATE ROUNDED
               DIVIDE TOTAL-CARDS-READ INTO TOTAL-SALARY
                   GIVING AVERAGE-SALARY ROUNDED
           ELSE
      * AVERAGES ALREADY SET TO 0
               NEXT SENTENCE.
           MOVE AVERAGE-PAY-RATE TO SUMMARY-PAY-RATE.
           MOVE AVERAGE-SALARY TO SUMMARY-SALARY.
           MOVE TOTAL-CARDS-READ TO SUMMARY-TOTAL-CARDS.
           WRITE PRINT-LINE FROM SUMMARY-LINE AFTER 3 LINES.
      *
       CLOSING.
           CLOSE PAY-FILE.
           CLOSE PRINT-FILE.
/*
//GO.INPUT DD *
JOHN SMITH               123-432-12ACCOUNTING     2540
MARY JOHNSON             555-123-45HR             1875
PETER MILLER             987-654-32IT             3250
SUSAN BROWN              111-222-33MARKETING      2210
ROBERT WILSON            444-555-66FINANCE        2895
LINDA DAVIS              777-888-99SALES          1950
MICHAEL TAYLOR           333-444-55OPERATIONS     2740
PATRICIA ANDERSON        666-777-88PURCHASING     2135
DAVID THOMAS             999-000-11ENGINEERING    3580
JENNIFER MOORE           222-333-44QUALITY        2475
JOSHUA WATSON            447-221-88BUDGET CONTROL 4856
LAUREN RODRIGUEZ         123-456-87MARKETING      5000
NATHAN SCOTT             654-852-85ACCOUNTING     7500
BENJAMIN PEREZ           159-753-56STRATEGIES     5600
ASHLEY WARD              612-782-12OPERATIONS     6250
EVELYN BELL              963-852-45PLANNING SPC   5875
GABRIEL ANDERSON         854-698-14RESEARCH       5687
TIMOTHY CARTER           654-852-99BRAND DIRECTOR 6352
OLIVER MOORE             112-254-56MARKETING      5345
OWEN WOOD                787-584-55IT SERVICES    6541
ROBERT ROSS              446-187-53RESEARCH       5988
HEATHER THOMAS           268-471-77IT SERVICES    5784
MEGAN SANDERS            784-885-69ACCOUNTING     6582
AUBREY COLLINS           456-753-12RESEARCH       5874
JAMES WASHINGTON         574-698-55FINANCE MANAGER8500
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//GO.SYSOUT DD SYSOUT=*
//
