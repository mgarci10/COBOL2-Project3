      ******************************************************************
      * PROGRAM : PROJECT 3                                            *
      * AUTHOR  : MARIO GARCIA                                         *
      * DUE DATE: 02/24/2022                                           *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARCIA-P03-PAY-SCREEN.

      *************************************************************

       ENVIRONMENT DIVISION.

      *************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PROG-DATE.
           03 WS-PROG-DATE-YEAR PIC 9999.
           03 WS-PROG-DATE-MONTH     PIC 99.
           03 WS-PROG-DATE-DAY       PIC 99.
           03 WS-PROG-DATE-TIME      PIC 9999.

       01 WS-SHOW-DATE.
           03 WS-SHOW-MONTH     PIC XX.
           03 FILLER            PIC X VALUE '/'.
           03 WS-SHOW-DAY       PIC XX.
           03 FILLER            PIC X VALUE '/'.
           03 WS-SHOW-YEAR      PIC XXXX.

       01 WS-WRK-INFO.
           03 WS-HRS-INPUT-1    PIC 9(2).
           03 WS-HRS-INPUT-2    PIC 9(2).
           03 WS-TOTAL-HRS      PIC 9(2)V99.
           03 WS-REG-HRS        PIC 9(2)V99.
           03 WS-OT-HRS         PIC 9(2)V99.

           03 WS-RATE-INPUT-1   PIC 9(2).
           03 WS-RATE-INPUT-2   PIC 9(2).
           03 WS-REG-RATE       PIC 9(2)V99.
           03 WS-OT-RATE        PIC 9(3)V99.

           03 WS-REG-EARNINGS   PIC 9(5)V99.
           03 WS-OT-EARNINGS    PIC 9(5)V99.
           03 WS-TOTAL-EARNINGS PIC 9(5)V99.

       01 WS-SCRN-CTL.
           03 WS-EXIT              PIC X VALUE SPACE.

       SCREEN SECTION.
       01 SCRN-HEADER.
           03 BLANK SCREEN.
           03 LINE 01 COL 01 VALUE 'SOLUTION'.
           03         COL 30 VALUE 'PAYROLL CALCULATION'.
           03         COL 71 PIC X(10) FROM WS-SHOW-DATE.

       01 SCRN-INPUT-FIELDS.
           03 SCRN-HRS-WRKED.
                05 LINE 06 COL 11 VALUE 'HOURS WORKED'.
                05         COL 26 PIC 9(2) TO WS-HRS-INPUT-1.
                05         COL 28 VALUE '.'.
                05         COL 29 PIC 9(2) TO WS-HRS-INPUT-2.

           03 SCRN-PAY-RATE.
                05 LINE 07 COL 11 VALUE 'PAY RATE'.
                05         COL 26 PIC 9(2) TO WS-RATE-INPUT-1.
                05         COL 28 VALUE '.'.
                05         COL 29 PIC 9(2) TO WS-RATE-INPUT-2.
       01 SCRN-CALCULATIONS.
           03 LINE 10 COL 34 VALUE 'HOURS     RATE   EARNINGS'.
           03 LINE 11 COL 21 VALUE 'REGULAR'.
           03         COL 34 PIC Z9.99 FROM WS-REG-HRS.
           03         COL 43 PIC Z9.99 FROM WS-REG-RATE.
           03         COL 50 PIC ZZ,ZZ9.99 FROM WS-REG-EARNINGS.
           03 LINE 12 COL 21 VALUE 'OVERTIME'.
           03         COL 34 PIC Z9.99 FROM WS-OT-HRS.
           03         COL 42 PIC ZZ9.99 FROM WS-OT-RATE.
           03         COL 50 PIC ZZ,ZZ9.99 FROM WS-OT-EARNINGS.
           03 LINE 14 COL 21 VALUE 'TOTAL'.
           03         COL 34 PIC Z9.99 FROM WS-TOTAL-HRS.
           03         COL 50 PIC ZZ,ZZ9.99 FROM WS-TOTAL-EARNINGS.


       01  SCRN-EXIT-OPTION.
           03  LINE 24 COL 22 PIC X TO WS-EXIT     AUTO.
           03          COL 24 VALUE
                   'PRESS ENTER TO CONTINUE (X=EXIT)'.
       01  SCRN-RESET.
           03  LINE 06 COL  1 ERASE EOS.

       01  SCRN-CLOSING.
           03  BLANK SCREEN.
           03  LINE 25 COL  1 VALUE SPACES.

      *************************************************************

       PROCEDURE DIVISION.
       360-MAIN-PROCEDURE.
           PERFORM 500-GET-THAT-DATE.
           DISPLAY SCRN-HEADER.
           PERFORM UNTIL WS-EXIT EQUALS 'X' OR 'x'
                DISPLAY SCRN-INPUT-FIELDS
                ACCEPT SCRN-HRS-WRKED
                ACCEPT SCRN-PAY-RATE
                PERFORM 101-FORMAT-INPUTS
                PERFORM 123-CALC-HRS
                PERFORM 456-CALC-RATE
                PERFORM 789-CALC-EARNINGS
      *      DISPLAY SCRN-CLOSING
      *          DISPLAY SCRN-HEADER
                DISPLAY SCRN-RESET
                DISPLAY SCRN-CALCULATIONS
                DISPLAY SCRN-EXIT-OPTION
                ACCEPT SCRN-EXIT-OPTION
                DISPLAY SCRN-RESET
           END-PERFORM.
           DISPLAY SCRN-CLOSING.
           STOP RUN.

      *************************************************************
       500-GET-THAT-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-PROG-DATE.
           MOVE WS-PROG-DATE-MONTH TO WS-SHOW-MONTH.
           MOVE WS-PROG-DATE-DAY TO WS-SHOW-DAY.
           MOVE WS-PROG-DATE-YEAR TO WS-SHOW-YEAR.

       101-FORMAT-INPUTS.
           COMPUTE WS-TOTAL-HRS = WS-HRS-INPUT-1 +
           (WS-HRS-INPUT-2 / 100).

           COMPUTE WS-REG-RATE = WS-RATE-INPUT-1 +
           (WS-RATE-INPUT-2 /100).

       123-CALC-HRS.
           MOVE ZEROS TO WS-OT-HRS.

           IF WS-TOTAL-HRS > 40.00 THEN
                COMPUTE WS-OT-HRS = WS-TOTAL-HRS - 40.00
                MOVE 40.00 TO WS-REG-HRS
           ELSE
                MOVE WS-TOTAL-HRS TO WS-REG-HRS
           END-IF.

       456-CALC-RATE.
           MOVE ZEROS TO WS-OT-RATE.

           IF WS-TOTAL-HRS > 40.00 THEN
                COMPUTE WS-OT-RATE ROUNDED = WS-REG-RATE * 1.5
           END-IF.

       789-CALC-EARNINGS.
           COMPUTE WS-REG-EARNINGS ROUNDED = WS-REG-RATE * WS-REG-HRS.
           COMPUTE WS-OT-EARNINGS ROUNDED = WS-OT-RATE * WS-OT-HRS.
           COMPUTE WS-TOTAL-EARNINGS
           = WS-REG-EARNINGS + WS-OT-EARNINGS.
