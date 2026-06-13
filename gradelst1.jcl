//COBOL JOB 1,'COBOL PROGRAM',CLASS=A,MSGCLASS=X
//COBOL  EXEC PROC=COBUCG
//COB.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-LIST.
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
           05  FILLER                  PIC X(5).
           05  STUDENT-SCORE1-IN       PIC 99.
           05  FILLER                  PIC X(3).
           05  STUDENT-SCORE2-IN       PIC 99.
           05  FILLER                  PIC X(3).
           05  STUDENT-GRADE-IN        PIC X.
           05  FILLER                  PIC X(29).
      *
       FD  PRINT-FILE
           LABEL RECORD IS OMITTED.
       01  PRINT-LINE.
           05  FILLER                  PIC X(21).
           05  STUDENT-NAME-OUT        PIC X(25).
           05  FILLER                  PIC X(5).
           05  STUDENT-ID-OUT          PIC X(10).
           05  FILLER                  PIC X(20).
           05  STUDENT-SCORE1-OUT      PIC 99.
           05  FILLER                  PIC X(4).
           05  STUDENT-SCORE2-OUT      PIC 99.
           05  FILLER                  PIC X(4).
           05  STUDENT-GRADE-OUT       PIC X.
      *
       WORKING-STORAGE SECTION.
      *
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM INITIALIZATION.
           PERFORM READ-AND-PRINT 20 TIMES.
           PERFORM CLOSING.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT GRADE-FILE,
                OUTPUT PRINT-FILE.
      *
       READ-AND-PRINT.
           MOVE SPACES TO PRINT-LINE.
           READ GRADE-FILE
               AT END STOP RUN.
           MOVE STUDENT-NAME-IN TO STUDENT-NAME-OUT.
           MOVE STUDENT-ID-IN TO STUDENT-ID-OUT.
           MOVE STUDENT-SCORE1-IN TO STUDENT-SCORE1-OUT.
           MOVE STUDENT-SCORE2-IN TO STUDENT-SCORE2-OUT.
           MOVE STUDENT-GRADE-IN TO STUDENT-GRADE-OUT.
           WRITE PRINT-LINE.
      *
       CLOSING.
           CLOSE GRADE-FILE, PRINT-FILE.
/*
//GO.INPUT DD *
447-221   JOSHUA WATSON                 38   38   A
123-456   LAUREN RODRIGUEZ              27   27   B
654-852   NATHAN SCOTT                  63   63   C
159-753   BENJAMIN PEREZ                87   87   D
612-782   ASHLEY WARD                   65   65   E
963-852   EVELYN BELL                   14   14   F
854-698   GABRIEL ANDERSON              23   23   A
654-852   TIMOTHY CARTER                25   25   B
112-254   OLIVER MOORE                  34   34   C
787-584   OWEN WOOD                     65   65   D
446-187   ROBERT ROSS                   47   47   E
268-471   HEATHER THOMAS                58   58   F
784-885   MEGAN SANDERS                 59   59   A
456-753   AUBREY COLLINS                88   88   B
574-698   JAMES WASHINGTON              63   63   C
456-587   SIMON SULSER                  48   48   D
875-965   JAMES BROWN                   66   66   E
212-236   KEVIN BLUE                    57   57   F
985-654   HUGH GRANT                    75   75   A
265-121   JAMES KING                    23   23   B
/*
//GO.OUTPUT DD SYSOUT=*,
//             DCB=(RECFM=FBA,LRECL=132,BLKSIZE=13200)
//
