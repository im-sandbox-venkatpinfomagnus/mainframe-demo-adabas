       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-READ.
       AUTHOR. MAINFRAME-DEMO.
      *================================================================
      * PROGRAM: ACCOUNT-READ
      * PURPOSE: Read and display customer account information
      * INPUT:   Account number from user
      * OUTPUT:  Complete account details
      *================================================================
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * Adabas Control Block
      *----------------------------------------------------------------
       01  ADABAS-CONTROL-BLOCK.
           05  ACB-COMMAND-CODE        PIC XX.
           05  ACB-COMMAND-ID          PIC X(04).
           05  ACB-FILE-NUMBER         PIC 9(05).
           05  ACB-RESPONSE-CODE       PIC 9(03).
           05  ACB-ISN                 PIC 9(10).
           05  ACB-ISN-LOWER-LIMIT     PIC 9(10).
           05  ACB-ISN-QUANTITY        PIC 9(10).
           05  ACB-FORMAT-BUFFER-LEN   PIC 9(05).
           05  ACB-RECORD-BUFFER-LEN   PIC 9(05).
           05  ACB-SEARCH-BUFFER-LEN   PIC 9(05).
           05  ACB-VALUE-BUFFER-LEN    PIC 9(05).
           05  ACB-ISN-BUFFER-LEN      PIC 9(05).
           05  FILLER                  PIC X(40).
       
      *----------------------------------------------------------------
      * Account Record Structure
      *----------------------------------------------------------------
       01  ACCOUNT-RECORD.
           05  ACCOUNT-NUMBER          PIC 9(10).
           05  CUSTOMER-NAME           PIC X(50).
           05  ACCOUNT-TYPE            PIC X(15).
           05  BALANCE                 PIC 9(09)V99.
           05  LAST-TXN-DATE           PIC 9(08).
       
      *----------------------------------------------------------------
      * Working Variables
      *----------------------------------------------------------------
       01  WS-INPUT-ACCOUNT            PIC 9(10).
       01  WS-CONTINUE                 PIC X VALUE 'Y'.
       01  WS-FORMATTED-BALANCE        PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-FORMATTED-DATE.
           05  WS-DATE-YYYY            PIC 9(04).
           05  FILLER                  PIC X VALUE '-'.
           05  WS-DATE-MM              PIC 99.
           05  FILLER                  PIC X VALUE '-'.
           05  WS-DATE-DD              PIC 99.
       
      *----------------------------------------------------------------
      * Format and Search Buffers
      *----------------------------------------------------------------
       01  FORMAT-BUFFER               PIC X(100) VALUE
           'ACCOUNT-NUMBER,CUSTOMER-NAME,ACCOUNT-TYPE,BALANCE,LAST-TXN-
      -    'DATE.'.
       
       01  SEARCH-BUFFER.
           05  FILLER                  PIC X(17) VALUE 
               'ACCOUNT-NUMBER,1,'.
           05  SB-ACCOUNT-NUMBER       PIC 9(10).
           05  FILLER                  PIC X VALUE '.'.
       
       01  RECORD-BUFFER               PIC X(500).
       
       PROCEDURE DIVISION.
       
      *----------------------------------------------------------------
       0000-MAIN-ROUTINE.
      *----------------------------------------------------------------
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNTS
               UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
           PERFORM 3000-TERMINATE
           STOP RUN.
       
      *----------------------------------------------------------------
       1000-INITIALIZE.
      *----------------------------------------------------------------
           DISPLAY '=================================================='
           DISPLAY 'CUSTOMER ACCOUNT INQUIRY SYSTEM'
           DISPLAY '=================================================='
           DISPLAY ' '
           
           MOVE 'L3'   TO ACB-COMMAND-CODE
           MOVE 'READ' TO ACB-COMMAND-ID
           MOVE 00012  TO ACB-FILE-NUMBER
           .
       
      *----------------------------------------------------------------
       2000-PROCESS-ACCOUNTS.
      *----------------------------------------------------------------
           PERFORM 2100-GET-ACCOUNT-NUMBER
           
           IF WS-INPUT-ACCOUNT > 0
               PERFORM 2200-READ-ACCOUNT
               IF ACB-RESPONSE-CODE = 000
                   PERFORM 2300-DISPLAY-ACCOUNT
               ELSE
                   PERFORM 2400-HANDLE-ERROR
               END-IF
           END-IF
           
           PERFORM 2500-ASK-CONTINUE
           .
       
      *----------------------------------------------------------------
       2100-GET-ACCOUNT-NUMBER.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'Enter Account Number (10 digits): ' NO ADVANCING
           ACCEPT WS-INPUT-ACCOUNT
           
           IF WS-INPUT-ACCOUNT = 0
               DISPLAY 'Invalid account number. Please try again.'
               DISPLAY ' '
           END-IF
           .
       
      *----------------------------------------------------------------
       2200-READ-ACCOUNT.
      *----------------------------------------------------------------
           MOVE WS-INPUT-ACCOUNT TO SB-ACCOUNT-NUMBER
           
           INITIALIZE ADABAS-CONTROL-BLOCK
           MOVE 'S1'   TO ACB-COMMAND-CODE
           MOVE 00012  TO ACB-FILE-NUMBER
           MOVE 100    TO ACB-FORMAT-BUFFER-LEN
           MOVE 500    TO ACB-RECORD-BUFFER-LEN
           MOVE 30     TO ACB-SEARCH-BUFFER-LEN
           
           CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                               FORMAT-BUFFER
                               RECORD-BUFFER
                               SEARCH-BUFFER
           
           IF ACB-RESPONSE-CODE = 000
               MOVE RECORD-BUFFER TO ACCOUNT-RECORD
           END-IF
           .
       
      *----------------------------------------------------------------
       2300-DISPLAY-ACCOUNT.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'ACCOUNT DETAILS'
           DISPLAY '=================================================='
           DISPLAY 'Account Number : ' ACCOUNT-NUMBER
           DISPLAY 'Customer Name  : ' CUSTOMER-NAME
           DISPLAY 'Account Type   : ' ACCOUNT-TYPE
           
           MOVE BALANCE TO WS-FORMATTED-BALANCE
           DISPLAY 'Balance        : $' WS-FORMATTED-BALANCE
           
           PERFORM 2310-FORMAT-DATE
           DISPLAY 'Last Trans Date: ' WS-FORMATTED-DATE
           DISPLAY '=================================================='
           .
       
      *----------------------------------------------------------------
       2310-FORMAT-DATE.
      *----------------------------------------------------------------
           MOVE LAST-TXN-DATE(1:4) TO WS-DATE-YYYY
           MOVE LAST-TXN-DATE(5:2) TO WS-DATE-MM
           MOVE LAST-TXN-DATE(7:2) TO WS-DATE-DD
           .
       
      *----------------------------------------------------------------
       2400-HANDLE-ERROR.
      *----------------------------------------------------------------
           DISPLAY ' '
           EVALUATE ACB-RESPONSE-CODE
               WHEN 003
                   DISPLAY 'ERROR: Account not found'
               WHEN 009
                   DISPLAY 'ERROR: File not available'
               WHEN 017
                   DISPLAY 'ERROR: Invalid file number'
               WHEN OTHER
                   DISPLAY 'ERROR: Adabas Response Code = '
                           ACB-RESPONSE-CODE
           END-EVALUATE
           DISPLAY ' '
           .
       
      *----------------------------------------------------------------
       2500-ASK-CONTINUE.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'Read another account? (Y/N): ' NO ADVANCING
           ACCEPT WS-CONTINUE
           .
       
      *----------------------------------------------------------------
       3000-TERMINATE.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'ACCOUNT INQUIRY SESSION ENDED'
           DISPLAY '=================================================='
           .
