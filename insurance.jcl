//COBOL JOB 1,'COBOL PROGRAM',CLASS=A,MSGCLASS=X
//COBOL  EXEC PROC=COBUCG
//COB.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSURANCE.
      *** 
       ENVIRONMENT DIVISION.
      * 
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE   ASSIGN TO UT-S-INPUT.
           SELECT OUTPUT-FILE  ASSIGN TO UT-S-OUTPUT.
      ***
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           LABEL RECORD OMITTED           
           RECORD CONTAINS 80 CHARACTERS.
       01  INPUT-REC.
           03  INP-NUMBER                  PIC X(10).
           03  INP-COMPANY                 PIC X(30).
           03  INP-AMOUNT                  PIC 9(7).
           03  FILLER                      PIC XX.
           03  INP-FACTOR1                 PIC 9V99.
           03  FILLER                      PIC XX.
           03  INP-FACTOR2                 PIC 9V99.
           03  FILLER                      PIC XX.
           03  INP-FACTOR3                 PIC 9V99.
           03  FILLER                      PIC X(18).
      * 
       FD  OUTPUT-FILE
           LABEL RECORD OMITTED.
       01  PRINT-LINE                      PIC X(132).
      *    
       WORKING-STORAGE SECTION.
       77  IS-EOF                          PIC X   VALUE 'N'.
       77  CARDS-READ                      PIC S9(4) COMP VALUE ZERO.
       01  WS-CALCULATIONS.
           03  TOTAL-NON-ACCIDENT          PIC S9(9) COMP VALUE ZERO.
           03  TOTAL-ACCIDENT              PIC S9(9) COMP VALUE ZERO.
           03  TOTAL-COMMON-CARRIER        PIC S9(9) COMP VALUE ZERO.
           03  AMOUNT-NON-ACCIDENT         PIC S9(9) COMP VALUE ZERO.
           03  AMOUNT-ACCIDENT             PIC S9(9) COMP VALUE ZERO.
           03  AMOUNT-COMMON-CARRIER       PIC S9(9) COMP VALUE ZERO.

      *
       01  HEADER-LINE.
           03  FILLER                      PIC X(51) VALUE SPACES.
           03  FILLER                      PIC X(30) VALUE
               'ANALYSIS OF INSURANCE COVERAGE'.
      *
       01  SUBTITLE1-LINE.
           03  FILLER                      PIC X(20) VALUE SPACES.
           03  FILLER                      PIC X(06) VALUE
               'POLICY'.
           03  FILLER                      PIC X(47) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE
               'PAYMENT IF'.
           03  FILLER                      PIC X(05) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE
               'PAYMENT IF'.
           03  FILLER                      PIC X(05) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE
               'PAYMENT IF'.
      *
       01  SUBTITLE2-LINE.
           03  FILLER                      PIC X(20) VALUE SPACES.
           03  FILLER                      PIC X(06) VALUE
               'NUMBER'.
           03  FILLER                      PIC X(09) VALUE SPACES.
           03  FILLER                      PIC X(17) VALUE
               'INSURANCE COMPANY'.
           03  FILLER                      PIC X(19) VALUE SPACES.
           03  FILLER                      PIC X(14) VALUE
               'NON-ACCIDENTAL'.         
           03  FILLER                      PIC X(04) VALUE SPACES.
           03  FILLER                      PIC X(08) VALUE
               'DEATH BY'.                        
           03  FILLER                      PIC X(07) VALUE SPACES.
           03  FILLER                      PIC X(08) VALUE
               'DEATH IN'.
      *
       01  SUBTITLE3-LINE.
           03  FILLER                      PIC X(74) VALUE SPACES.
           03  FILLER                      PIC X(05) VALUE
               'DEATH'.
           03  FILLER                      PIC X(09) VALUE SPACES.
           03  FILLER                      PIC X(08) VALUE
               'ACCIDENT'.
           03  FILLER                      PIC X(04) VALUE SPACES.
           03  FILLER                      PIC X(15) VALUE
               'COMMON CARRIERS'.
      *  
       01  DETAIL-LINE.
           03  FILLER                      PIC X(20) VALUE SPACES.
           03  DETAIL-NUMBER               PIC X(10) VALUE SPACES.
           03  FILLER                      PIC X(05) VALUE SPACES.
           03  DETAIL-COMPANY              PIC X(30) VALUE SPACES.
           03  FILLER                      PIC X(06) VALUE SPACES.
           03  DETAIL-NON-ACCIDENT         PIC Z,ZZZ,ZZ9 VALUE ZERO.
           03  FILLER                      PIC X(06) VALUE SPACES.
           03  DETAIL-ACCIDENT             PIC Z,ZZZ,ZZ9 VALUE ZERO.
           03  FILLER                      PIC X(06) VALUE SPACES.
           03  DETAIL-COMMON-CARRIER       PIC Z,ZZZ,ZZ9 VALUE ZERO.
      *
       01  SUMMARY-LINE.
           03  FILLER                      PIC X(54) VALUE SPACES.
           03  FILLER                      PIC X(16) VALUE
               'TOTAL PAID      '.
           03  SUMMARY-NON-ACCIDENT        PIC ZZ,ZZZ,ZZ9 VALUE ZERO.
           03  FILLER                      PIC X(06) VALUE SPACES.
           03  SUMMARY-ACCIDENT            PIC ZZ,ZZZ,ZZ9 VALUE ZERO.
           03  FILLER                      PIC X(04) VALUE SPACES.
           03  SUMMARY-COMMON-CARRIER      PIC ZZ,ZZZ,ZZ9 VALUE ZERO.
      ***
       PROCEDURE DIVISION.
       START-PROC.
           PERFORM INITIALIZATION THRU INITIALIZATION-EXIT.
           PERFORM READ-AND-PRINT THRU READ-AND-PRINT-EXIT
               UNTIL IS-EOF = 'Y' OR CARDS-READ = 30.
           PERFORM FINALIZATION.
           STOP RUN.
      *
       INITIALIZATION.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
      * PRINTING HEADERS
           WRITE PRINT-LINE FROM HEADER-LINE AFTER 1 LINES.
           WRITE PRINT-LINE FROM SUBTITLE1-LINE AFTER 2 LINES.
           WRITE PRINT-LINE FROM SUBTITLE2-LINE AFTER 1 LINES.
           WRITE PRINT-LINE FROM SUBTITLE3-LINE AFTER 1 LINES.
           READ INPUT-FILE
               AT END MOVE 'Y' TO IS-EOF.
       INITIALIZATION-EXIT.
           EXIT.
      *
       READ-AND-PRINT.
           MOVE INP-NUMBER TO DETAIL-NUMBER.
           MOVE INP-COMPANY TO DETAIL-COMPANY.
      *
           MULTIPLY INP-AMOUNT BY INP-FACTOR1
               GIVING AMOUNT-NON-ACCIDENT. 
           MOVE AMOUNT-NON-ACCIDENT TO DETAIL-NON-ACCIDENT.
           ADD AMOUNT-NON-ACCIDENT TO TOTAL-NON-ACCIDENT.
      *
           MULTIPLY INP-AMOUNT BY INP-FACTOR2
               GIVING AMOUNT-ACCIDENT.
           MOVE AMOUNT-ACCIDENT TO DETAIL-ACCIDENT.
           ADD AMOUNT-ACCIDENT TO TOTAL-ACCIDENT.
      *
           MULTIPLY INP-AMOUNT BY INP-FACTOR3
               GIVING AMOUNT-COMMON-CARRIER.
           MOVE AMOUNT-COMMON-CARRIER TO DETAIL-COMMON-CARRIER.
           ADD AMOUNT-COMMON-CARRIER TO TOTAL-COMMON-CARRIER.
      *
           WRITE PRINT-LINE FROM DETAIL-LINE AFTER 1 LINES.
           ADD 1 TO CARDS-READ.
           READ INPUT-FILE
               AT END MOVE 'Y' TO IS-EOF.
       READ-AND-PRINT-EXIT.
           EXIT.
      *
       FINALIZATION.
           MOVE TOTAL-NON-ACCIDENT TO SUMMARY-NON-ACCIDENT.
           MOVE TOTAL-ACCIDENT TO SUMMARY-ACCIDENT.
           MOVE TOTAL-COMMON-CARRIER TO SUMMARY-COMMON-CARRIER.
           WRITE PRINT-LINE FROM SUMMARY-LINE AFTER 2 LINES.
           CLOSE OUTPUT-FILE.
           CLOSE INPUT-FILE.
/*
//GO.INPUT DD *
123-454341ALPHA INDUSTRIES              0012500  125  085  110
432-782332BETA CONSULTING LTD           0008750  150  095  105
634-830003GAMMA ELECTRONICS             0021500  175  120  090
543-840004DELTA SOFTWARE AG             0006500  110  100  125
683-730005EPSILON SERVICES              0014800  135  115  105
873-930006ZETA MANUFACTURING            0032500  200  140  115
649-580007ETA LOGISTICS                 0019750  145  110  130
674-340008THETA ENERGY GROUP            0045000  180  135  120
654-320009IOTA PHARMACEUTICALS          0028500  155  125  100
261-890010KAPPA RETAIL SYSTEMS          0011250  120  090  085
231-780011LAMBDA CONSTRUCTION           0038750  195  145  135
943-330012MU FINANCIAL SERVICES         0024750  140  105  115
895-650013NU INSURANCE GROUP            0018900  130  110  120
783-480014XI FOOD PRODUCTS              0022250  160  115  095
732-110015OMICRON TELECOM               0041250  185  150  125
465-370016PI MEDICAL EQUIPMENT          0034500  175  135  140
241-870017RHO AVIATION SYSTEMS          0052500  210  160  145
325-650018SIGMA AUTOMATION              0027750  165  120  110
847-460019TAU RESEARCH LABS             0031250  170  130  135
829-780020UPSILON DATA SOLUTIONS        0048750  190  155  150
/*
//GO.OUTPUT DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300)
//
