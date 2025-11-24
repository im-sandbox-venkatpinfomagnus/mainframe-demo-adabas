       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-UPDATE.
       AUTHOR. MAINFRAME-DEMO.
      *================================================================
      * PROGRAM: ACCOUNT-UPDATE
      * PURPOSE: Update customer account balance and transaction date
      * INPUT:   Account number and new balance from user
      * OUTPUT:  Confirmation of update or error message
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
       01  WS-TRANSACTION-TYPE         PIC X.
           88  DEPOSIT                 VALUE 'D'.
           88  WITHDRAWAL              VALUE 'W'.
           88  BALANCE-ADJUSTMENT      VALUE 'A'.
       01  WS-AMOUNT                   PIC 9(09)V99.
       01  WS-NEW-BALANCE              PIC 9(09)V99.
       01  WS-OLD-BALANCE              PIC 9(09)V99.
       01  WS-CONTINUE                 PIC X VALUE 'Y'.
       01  WS-CONFIRM                  PIC X.
       01  WS-SAVED-ISN                PIC 9(10).
       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR            PIC 9(04).
           05  WS-CURR-MONTH           PIC 99.
           05  WS-CURR-DAY             PIC 99.
       01  WS-NEW-TXN-DATE             PIC 9(08).
       
      *----------------------------------------------------------------
      * Display Formatting
      *----------------------------------------------------------------
       01  WS-FORMATTED-BALANCE        PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-FORMATTED-AMOUNT         PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-FORMATTED-DATE.
           05  WS-DATE-YYYY            PIC 9(04).
           05  FILLER                  PIC X VALUE '-'.
           05  WS-DATE-MM              PIC 99.
           05  FILLER                  PIC X VALUE '-'.
           05  WS-DATE-DD              PIC 99.
       
      *----------------------------------------------------------------
      * Format and Search Buffers
      *----------------------------------------------------------------
       01  READ-FORMAT-BUFFER          PIC X(100) VALUE
           'ACCOUNT-NUMBER,CUSTOMER-NAME,ACCOUNT-TYPE,BALANCE,LAST-TXN-
      -    'DATE.'.
       
       01  UPDATE-FORMAT-BUFFER        PIC X(50) VALUE
           'BALANCE,LAST-TXN-DATE.'.
       
       01  SEARCH-BUFFER.
           05  FILLER                  PIC X(17) VALUE 
               'ACCOUNT-NUMBER,1,'.
           05  SB-ACCOUNT-NUMBER       PIC 9(10).
           05  FILLER                  PIC X VALUE '.'.
       
       01  RECORD-BUFFER               PIC X(500).
       01  UPDATE-BUFFER.
           05  UB-BALANCE              PIC 9(09)V99.
           05  UB-LAST-TXN-DATE        PIC 9(08).
       
       PROCEDURE DIVISION.
       
      *----------------------------------------------------------------
       0000-MAIN-ROUTINE.
      *----------------------------------------------------------------
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-UPDATES
               UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
           PERFORM 3000-TERMINATE
           STOP RUN.
       
      *----------------------------------------------------------------
       1000-INITIALIZE.
      *----------------------------------------------------------------
           DISPLAY '=================================================='
           DISPLAY 'CUSTOMER ACCOUNT UPDATE SYSTEM'
           DISPLAY '=================================================='
           DISPLAY ' '
           
           MOVE 00012 TO ACB-FILE-NUMBER
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-NEW-TXN-DATE
           .
       
      *----------------------------------------------------------------
       2000-PROCESS-UPDATES.
      *----------------------------------------------------------------
           PERFORM 2100-GET-ACCOUNT-NUMBER
           
           IF WS-INPUT-ACCOUNT > 0
               PERFORM 2200-READ-ACCOUNT
               IF ACB-RESPONSE-CODE = 000
                   PERFORM 2300-DISPLAY-CURRENT-INFO
                   PERFORM 2400-GET-UPDATE-INFO
                   IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y'
                       PERFORM 2500-UPDATE-ACCOUNT
                       IF ACB-RESPONSE-CODE = 000
                           PERFORM 2600-DISPLAY-SUCCESS
                       ELSE
                           PERFORM 2700-HANDLE-ERROR
                       END-IF
                   ELSE
                       DISPLAY 'Update cancelled.'
                   END-IF
               ELSE
                   PERFORM 2700-HANDLE-ERROR
               END-IF
           END-IF
           
           PERFORM 2800-ASK-CONTINUE
           .
       
      *----------------------------------------------------------------
       2100-GET-ACCOUNT-NUMBER.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'Enter Account Number to Update (10 digits): '
               NO ADVANCING
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
                               READ-FORMAT-BUFFER
                               RECORD-BUFFER
                               SEARCH-BUFFER
           
           IF ACB-RESPONSE-CODE = 000
               MOVE RECORD-BUFFER TO ACCOUNT-RECORD
               MOVE BALANCE TO WS-OLD-BALANCE
               MOVE ACB-ISN TO WS-SAVED-ISN
           END-IF
           .
       
      *----------------------------------------------------------------
       2300-DISPLAY-CURRENT-INFO.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'CURRENT ACCOUNT INFORMATION'
           DISPLAY '=================================================='
           DISPLAY 'Account Number : ' ACCOUNT-NUMBER
           DISPLAY 'Customer Name  : ' CUSTOMER-NAME
           DISPLAY 'Account Type   : ' ACCOUNT-TYPE
           
           MOVE BALANCE TO WS-FORMATTED-BALANCE
           DISPLAY 'Current Balance: $' WS-FORMATTED-BALANCE
           
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
       2400-GET-UPDATE-INFO.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'Transaction Type:'
           DISPLAY '  D - Deposit'
           DISPLAY '  W - Withdrawal'
           DISPLAY '  A - Balance Adjustment'
           DISPLAY 'Enter Choice (D/W/A): ' NO ADVANCING
           ACCEPT WS-TRANSACTION-TYPE
           
           DISPLAY ' '
           DISPLAY 'Enter Amount: ' NO ADVANCING
           ACCEPT WS-AMOUNT
           
           EVALUATE TRUE
               WHEN DEPOSIT
                   COMPUTE WS-NEW-BALANCE = WS-OLD-BALANCE + WS-AMOUNT
                   DISPLAY ' '
                   DISPLAY 'Deposit of $' WS-AMOUNT
               WHEN WITHDRAWAL
                   IF WS-AMOUNT > WS-OLD-BALANCE
                       DISPLAY ' '
                       DISPLAY 'ERROR: Insufficient funds'
                       DISPLAY 'Available balance: $' WS-OLD-BALANCE
                       MOVE 'N' TO WS-CONFIRM
                       EXIT PARAGRAPH
                   ELSE
                       COMPUTE WS-NEW-BALANCE = 
                           WS-OLD-BALANCE - WS-AMOUNT
                       DISPLAY ' '
                       DISPLAY 'Withdrawal of $' WS-AMOUNT
                   END-IF
               WHEN BALANCE-ADJUSTMENT
                   MOVE WS-AMOUNT TO WS-NEW-BALANCE
                   DISPLAY ' '
                   DISPLAY 'Balance adjustment to $' WS-AMOUNT
               WHEN OTHER
                   DISPLAY 'Invalid transaction type'
                   MOVE 'N' TO WS-CONFIRM
                   EXIT PARAGRAPH
           END-EVALUATE
           
           MOVE WS-NEW-BALANCE TO WS-FORMATTED-BALANCE
           DISPLAY 'New Balance will be: $' WS-FORMATTED-BALANCE
           DISPLAY ' '
           DISPLAY 'Confirm update? (Y/N): ' NO ADVANCING
           ACCEPT WS-CONFIRM
           .
       
      *----------------------------------------------------------------
       2500-UPDATE-ACCOUNT.
      *----------------------------------------------------------------
           MOVE WS-NEW-BALANCE TO UB-BALANCE
           MOVE WS-NEW-TXN-DATE TO UB-LAST-TXN-DATE
           
           INITIALIZE ADABAS-CONTROL-BLOCK
           MOVE 'A1'   TO ACB-COMMAND-CODE
           MOVE 00012  TO ACB-FILE-NUMBER
           MOVE WS-SAVED-ISN TO ACB-ISN
           MOVE 50     TO ACB-FORMAT-BUFFER-LEN
           MOVE 20     TO ACB-RECORD-BUFFER-LEN
           
           CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                               UPDATE-FORMAT-BUFFER
                               UPDATE-BUFFER
           .
       
      *----------------------------------------------------------------
       2600-DISPLAY-SUCCESS.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'UPDATE SUCCESSFUL'
           DISPLAY '=================================================='
           DISPLAY 'Account Number : ' ACCOUNT-NUMBER
           DISPLAY 'Customer Name  : ' CUSTOMER-NAME
           
           MOVE WS-OLD-BALANCE TO WS-FORMATTED-BALANCE
           DISPLAY 'Old Balance    : $' WS-FORMATTED-BALANCE
           
           MOVE WS-NEW-BALANCE TO WS-FORMATTED-BALANCE
           DISPLAY 'New Balance    : $' WS-FORMATTED-BALANCE
           
           MOVE WS-AMOUNT TO WS-FORMATTED-AMOUNT
           EVALUATE TRUE
               WHEN DEPOSIT
                   DISPLAY 'Transaction    : Deposit of $'
                           WS-FORMATTED-AMOUNT
               WHEN WITHDRAWAL
                   DISPLAY 'Transaction    : Withdrawal of $'
                           WS-FORMATTED-AMOUNT
               WHEN BALANCE-ADJUSTMENT
                   DISPLAY 'Transaction    : Balance Adjustment'
           END-EVALUATE
           
           DISPLAY 'Update Date    : ' WS-NEW-TXN-DATE
           DISPLAY '=================================================='
           .
       
      *----------------------------------------------------------------
       2700-HANDLE-ERROR.
      *----------------------------------------------------------------
           DISPLAY ' '
           EVALUATE ACB-RESPONSE-CODE
               WHEN 003
                   DISPLAY 'ERROR: Account not found'
               WHEN 009
                   DISPLAY 'ERROR: File not available'
               WHEN 017
                   DISPLAY 'ERROR: Invalid file number'
               WHEN 044
                   DISPLAY 'ERROR: Record locked by another user'
               WHEN 145
                   DISPLAY 'ERROR: Update not allowed'
               WHEN OTHER
                   DISPLAY 'ERROR: Adabas Response Code = '
                           ACB-RESPONSE-CODE
           END-EVALUATE
           DISPLAY ' '
           .
       
      *----------------------------------------------------------------
       2800-ASK-CONTINUE.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY 'Update another account? (Y/N): ' NO ADVANCING
           ACCEPT WS-CONTINUE
           .
       
      *----------------------------------------------------------------
       3000-TERMINATE.
      *----------------------------------------------------------------
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'ACCOUNT UPDATE SESSION ENDED'
           DISPLAY '=================================================='
           .
