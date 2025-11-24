      *****************************************************************
      * PROGRAM: CALCULATOR                                           *
      * PURPOSE: Simple arithmetic calculator demonstration           *
      * AUTHOR:  Mainframe Demo Project                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST-NUMBER     PIC 9(09)V99.
       01  WS-SECOND-NUMBER    PIC 9(09)V99.
       01  WS-RESULT           PIC 9(09)V99.
       01  WS-OPERATION        PIC X.
           88  WS-ADD          VALUE 'A'.
           88  WS-SUBTRACT     VALUE 'S'.
           88  WS-MULTIPLY     VALUE 'M'.
           88  WS-DIVIDE       VALUE 'D'.
       01  WS-DISPLAY-RESULT   PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-CONTINUE-FLAG    PIC X VALUE 'Y'.
           88  WS-CONTINUE     VALUE 'Y'.
           88  WS-EXIT         VALUE 'N'.
       
       PROCEDURE DIVISION.
       main-process.
           PERFORM display-welcome
           PERFORM UNTIL WS-EXIT
               PERFORM get-first-number
               PERFORM get-operation
               PERFORM get-second-number
               PERFORM calculate-result
               PERFORM display-result
               PERFORM ask-continue
           END-PERFORM
           PERFORM display-goodbye
           STOP RUN
           .
       
       display-welcome.
           DISPLAY '========================================='
           DISPLAY '      COBOL CALCULATOR PROGRAM          '
           DISPLAY '========================================='
           DISPLAY ' '
           .
       
       get-first-number.
           DISPLAY 'Enter first number: ' WITH NO ADVANCING
           ACCEPT WS-FIRST-NUMBER
           .
       
       get-operation.
           DISPLAY 'Select operation:'
           DISPLAY '  A - Addition'
           DISPLAY '  S - Subtraction'
           DISPLAY '  M - Multiplication'
           DISPLAY '  D - Division'
           DISPLAY 'Enter choice: ' WITH NO ADVANCING
           ACCEPT WS-OPERATION
           .
       
       get-second-number.
           DISPLAY 'Enter second number: ' WITH NO ADVANCING
           ACCEPT WS-SECOND-NUMBER
           .
       
       calculate-result.
           EVALUATE TRUE
               WHEN WS-ADD
                   ADD WS-FIRST-NUMBER TO WS-SECOND-NUMBER
                       GIVING WS-RESULT
               WHEN WS-SUBTRACT
                   SUBTRACT WS-SECOND-NUMBER FROM WS-FIRST-NUMBER
                       GIVING WS-RESULT
               WHEN WS-MULTIPLY
                   MULTIPLY WS-FIRST-NUMBER BY WS-SECOND-NUMBER
                       GIVING WS-RESULT
               WHEN WS-DIVIDE
                   IF WS-SECOND-NUMBER = ZEROS
                       DISPLAY 'Error: Cannot divide by zero'
                       MOVE ZEROS TO WS-RESULT
                   ELSE
                       DIVIDE WS-FIRST-NUMBER BY WS-SECOND-NUMBER
                           GIVING WS-RESULT
                   END-IF
               WHEN OTHER
                   DISPLAY 'Error: Invalid operation'
                   MOVE ZEROS TO WS-RESULT
           END-EVALUATE
           .
       
       display-result.
           MOVE WS-RESULT TO WS-DISPLAY-RESULT
           DISPLAY ' '
           DISPLAY 'Result: ' WS-DISPLAY-RESULT
           DISPLAY ' '
           .
       
       ask-continue.
           DISPLAY 'Perform another calculation? (Y/N): '
               WITH NO ADVANCING
           ACCEPT WS-CONTINUE-FLAG
           .
       
       display-goodbye.
           DISPLAY ' '
           DISPLAY 'Thank you for using COBOL Calculator!'
           DISPLAY '========================================='
           .
