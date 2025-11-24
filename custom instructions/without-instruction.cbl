       *> Generate COBOL code for calculator functionality
         IDENTIFICATION DIVISION.
            PROGRAM-ID. CALCULATOR.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01  WS-FIRST-NUMBER     PIC 9(09)V99.
         01  WS-SECOND-NUMBER    PIC 9(09)V99.
         01  WS-RESULT           PIC 9(09)V99.
         PROCEDURE DIVISION.
            main-process.
                DISPLAY 'Enter first number: ' WITH NO ADVANCING.
                ACCEPT WS-FIRST-NUMBER.
                
                DISPLAY 'Enter second number: ' WITH NO ADVANCING.
                ACCEPT WS-SECOND-NUMBER.
                
                ADD WS-FIRST-NUMBER TO WS-SECOND-NUMBER GIVING WS-RESULT.
                
                DISPLAY 'The result is: ' WS-RESULT.
                
                STOP RUN.
