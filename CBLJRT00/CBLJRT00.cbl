       identification division.
       program-id. CBLJRT00.

       environment division.
           select student-master
               assign to
               'C:\COBOLWI19\STDNTMST.DAT'
                   organization is line sequential.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOLWI19\STDNTRPT.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.

       data division.
       file section.
       FD  student-master
           label record is standard
           data record is I-Rec
           record contains 49 characters.

       01  I-Rec.
           05 I-ID                     pic X(7).
           05 I-Name.                  
               10 I-LNAME              PIC X(15).
               10 I-FNAME              PIC X(15).
               10 I-INIT               PIC X.
           05 I-GPA                    PIC 9V99.
           05 I-EX-STRT-SAL            PIC 9(6)V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                     PIC X(132).

       working-storage section.
       01  Misc.
           05  EOF                     Pic X(5)    value 'TRUE '.
           05  PAGE-CTR                PIC 99      VALUE 0.
           05  C-STUD-CTR              PIC 999     VALUE 0.
           05  CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR        PIC X(4).
               10  CURRENT-MONTH       PIC XX.
               10  CURRENT-DAY         PIC XX.
               10  CURRENT-TIME        PIC X(11).
       01  TITLE-LINE.
           05  FILLER                  PIC X(6)    VALUE 'DATE'.
           05  TITLE-DATE.
               10  TITLE-MONTH         PIC XX.
               10  FILLER              PIC X       VALUE '/'.
               10  TITLE-DAY           PIC XX.
               10  FILLER              PIC X       VALUE '/'.
               10  TITLE-YEAR          PIC X(4).
           05  FILLER                  PIC X(35)   VALUE SPACES.
           05  FILLER                  PIC X(29)
               VALUE 'WILSON S COBOL STUDENT ROSTER'.
           05  FILLER                  PIC X(44)   VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'PAGE: '.
           05  TITLE-PAGE              PIC Z9.

       01  COL-HEADING.
           05  FILLER                  PIC X(119)  VALUE SPACES.
           05  FILLER                  PIC X(11)   VALUE 'ANTICIPATED'.
           05  FILLER                  PIC XX      VALUE SPACES.

       01  COL-HEADING2.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  FILLER                  PIC XX      VALUE 'ID'.
           05  FILLER                  PIC X(23)   VALUE SPACES.
           05  FILLER                  PIC X(9)    VALUE 'LAST NAME'.
           05  FILLER                  PIC X(26)   VALUE SPACES.
           05  FILLER                  PIC X(10)   VALUE 'FIRST NAME'.
           05  FILLER                  PIC X(26)   VALUE SPACES.
           05  FILLER                  PIC XXX     VALUE 'GPA'.
           05  FILLER                  PIC X(16)   VALUE SPACES.
           05  FILLER                  PIC X(15)
               VALUE 'STARTING SALARY'.

       01  DETAIL-LINE.
           05  D-ID                    PIC X(7).
           05  FILLER                  PIC X(20)   VALUE SPACES.
           05  D-LAST-NAME             PIC X(15).
           05  FILLER                  PIC X(20)   VALUE SPACES.
           05  D-FIRST-NAME            PIC X(15).
           05  FILLER                  PIC X(20)   VALUE SPACES.
           05  D-GPA                   PIC Z.99.
           05  FILLER                  PIC X(18)   VALUE SPACES.
           05  D-STARTING-SALARY       PIC $ZZZ,ZZZ.99.
           05  FILLER                  PIC XX      VALUE SPACES.

       01  TOTAL-LINE.
           05  FILLER                  PIC X(54)   VALUE SPACES.
           05  FILLER                  PIC X(15)
               VALUE 'STUDENT COUNT: '.
           05  T-TOTAL-COUNT           PIC ZZ9.
           05  FILLER                  PIC X(60)   VALUE SPACES.


       procedure division.
       L1-Main.
           perform L2-Init.
           perform L2-Mainline
               Until EOF = 'FALSE'.
           perform L2-Closing.
           stop run.

       L2-Init.
           open input student-master.
           OPEN OUTPUT PRTOUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH  TO  TITLE-MONTH.
           MOVE CURRENT-DAY    TO  TITLE-DAY.
           MOVE CURRENT-YEAR   TO  TITLE-YEAR.
           PERFORM L4-HEADING.
           PERFORM L3-READ-INPUT.

       L2-Mainline.
           PERFORM L3-CALCS.
           PERFORM L3-MOVE-PRINT.
           PERFORM L3-READ-INPUT.

       L2-Closing.
           PERFORM L3-TOTALS.
           CLOSE STUDENT-MASTER.
           CLOSE PRTOUT.

       L3-CALCS.
           COMPUTE C-STUD-CTR = C-STUD-CTR + 1.
      *        OR
      *    ADD 1 TO C-STUD-CTR.

       L3-MOVE-PRINT.
           MOVE I-ID           TO D-ID.
           MOVE I-FNAME        TO D-FIRST-NAME.
           MOVE I-LNAME        TO D-LAST-NAME.
           MOVE I-GPA          TO D-GPA.
           MOVE I-EX-STRT-SAL  TO D-STARTING-SALARY.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM L4-HEADING.

       L3-READ-INPUT.
           READ STUDENT-MASTER
               AT END
                   MOVE 'FALSE' TO EOF.
       L3-TOTALS.
           MOVE C-STUD-CTR TO T-TOTAL-COUNT.
           WRITE PRTLINE FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES.

       L4-HEADING.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE
             AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COL-HEADING
             AFTER ADVANCING 2 LINE.
           WRITE PRTLINE FROM COL-HEADING2
             AFTER ADVANCING 1 LINE.

       end program CBLJRT00.