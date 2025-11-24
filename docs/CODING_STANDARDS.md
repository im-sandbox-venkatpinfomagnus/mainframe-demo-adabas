# COBOL and Adabas Coding Standards

## Table of Contents
1. [COBOL Coding Standards](#cobol-coding-standards)
2. [Adabas Specific Standards](#adabas-specific-standards)
3. [Program Structure](#program-structure)
4. [Naming Conventions](#naming-conventions)
5. [Error Handling](#error-handling)
6. [Documentation](#documentation)

---

## COBOL Coding Standards

### Format and Layout

#### Fixed-Format COBOL
```cobol
      * Column 1-6:   Sequence numbers (optional, can be blank)
      * Column 7:     Indicator area (* for comments, - for continuation)
      * Column 8-11:  Area A (Division, Section, Paragraph, 01/77 levels)
      * Column 12-72: Area B (All other statements)
      * Column 73-80: Identification area (optional)
```

**Example:**
```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ACCOUNT-READ.
000300*================================================================
000400* PROGRAM: ACCOUNT-READ
000500* PURPOSE: Read and display customer account information
000600*================================================================
000700 
000800 ENVIRONMENT DIVISION.
```

#### Indentation Rules
- Division headers: Column 8
- Section headers: Column 8
- Paragraph names: Column 8
- Level 01/77: Column 8
- Level 05-49: Indent 4 spaces per level
- Statements: Column 12 minimum
- Continuation lines: Column 12, use hyphen (-) in column 7

**Example:**
```cobol
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID         PIC 9(10).
           05  CUSTOMER-NAME.
               10  FIRST-NAME      PIC X(20).
               10  LAST-NAME       PIC X(30).
           05  ACCOUNT-BALANCE     PIC 9(09)V99.
```

### Naming Conventions

#### Program Names
- Use descriptive names with hyphens
- Maximum 8 characters for mainframe compatibility
- Format: `FUNCTION-VERB` or `ENTITY-ACTION`
```cobol
PROGRAM-ID. ACCT-READ.
PROGRAM-ID. CUST-UPD.
PROGRAM-ID. BAL-CALC.
```

#### Data Names
- Use meaningful, self-documenting names
- Use hyphens to separate words (no underscores in mainframe COBOL)
- Working Storage variables: prefix with `WS-`
- Linkage Section variables: prefix with `LS-`
- File Section variables: prefix with `FS-`

```cobol
WORKING-STORAGE SECTION.
01  WS-CUSTOMER-COUNT       PIC 9(05) VALUE ZERO.
01  WS-CURRENT-DATE         PIC 9(08).
01  WS-ERROR-FLAG           PIC X VALUE 'N'.
    88  WS-ERROR-FOUND      VALUE 'Y'.
    88  WS-NO-ERROR         VALUE 'N'.
```

#### Paragraph Names
- Use descriptive action-oriented names
- Prefix with numeric sequence for organization
- Format: `NNNN-DESCRIPTION`

```cobol
0000-MAIN-ROUTINE.
1000-INITIALIZE-PROGRAM.
2000-PROCESS-ACCOUNTS.
2100-READ-ACCOUNT-RECORD.
2200-VALIDATE-ACCOUNT-DATA.
2300-DISPLAY-ACCOUNT-INFO.
3000-FINALIZE-PROGRAM.
```

### Data Definitions

#### Numeric Data
```cobol
* Packed decimal for monetary values (recommended for Adabas)
01  WS-ACCOUNT-BALANCE      PIC S9(09)V99 COMP-3.

* Binary for counters and subscripts
01  WS-TABLE-INDEX          PIC S9(04) COMP.

* Display numeric for external data
01  WS-ACCOUNT-NUMBER       PIC 9(10).
```

#### Alphanumeric Data
```cobol
* Use appropriate length, avoid excess padding
01  WS-CUSTOMER-NAME        PIC X(50).
01  WS-ACCOUNT-TYPE         PIC X(15).

* Use VALUE clause for constants
01  WS-COMPANY-NAME         PIC X(30) VALUE 'ACME BANK CORPORATION'.
```

#### Level 88 Condition Names
```cobol
01  WS-ACCOUNT-STATUS       PIC X.
    88  ACTIVE-ACCOUNT      VALUE 'A'.
    88  CLOSED-ACCOUNT      VALUE 'C'.
    88  SUSPENDED-ACCOUNT   VALUE 'S'.
    88  VALID-STATUS        VALUE 'A' 'C' 'S'.
```

### Program Structure

#### Standard Program Template
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGNAME.
      *================================================================
      * PROGRAM: PROGNAME
      * AUTHOR: [Developer Name]
      * DATE WRITTEN: YYYY-MM-DD
      * PURPOSE: [Brief description]
      * INPUT: [Input sources]
      * OUTPUT: [Output destinations]
      * DEPENDENCIES: [Called programs, files]
      *================================================================
      * MODIFICATION HISTORY:
      * DATE       AUTHOR      DESCRIPTION
      * ---------- ----------- ---------------------------------------
      * YYYY-MM-DD [Name]      Initial version
      *================================================================
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           [File definitions]
       
       DATA DIVISION.
       FILE SECTION.
           [File record layouts]
       
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-VARIABLES.
           [Working variables]
       
       LINKAGE SECTION.
           [Parameters passed to/from program]
       
       PROCEDURE DIVISION.
       
      *----------------------------------------------------------------
       0000-MAIN-ROUTINE.
      *----------------------------------------------------------------
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-LOGIC
           PERFORM 3000-FINALIZE
           STOP RUN.
```

### Control Flow

#### Use Structured Programming
```cobol
* GOOD: Structured IF-ELSE
IF WS-BALANCE > WS-MINIMUM-BALANCE
    PERFORM 2100-PROCESS-ACTIVE-ACCOUNT
ELSE
    IF WS-BALANCE = ZERO
        PERFORM 2200-CLOSE-ACCOUNT
    ELSE
        PERFORM 2300-FLAG-LOW-BALANCE
    END-IF
END-IF

* AVOID: GO TO statements (use only for error handling)
* GOOD: Use PERFORM for flow control
PERFORM 2000-PROCESS-ACCOUNTS
    UNTIL WS-EOF = 'Y' OR WS-ERROR-FOUND
```

#### EVALUATE vs Nested IFs
```cobol
* PREFER: EVALUATE for multiple conditions
EVALUATE TRUE
    WHEN CHECKING-ACCOUNT
        PERFORM 2100-PROCESS-CHECKING
    WHEN SAVINGS-ACCOUNT
        PERFORM 2200-PROCESS-SAVINGS
    WHEN FIXED-DEPOSIT-ACCOUNT
        PERFORM 2300-PROCESS-FIXED-DEPOSIT
    WHEN OTHER
        PERFORM 9000-INVALID-ACCOUNT-TYPE
END-EVALUATE
```

---

## Adabas Specific Standards

### Adabas Control Block (ACB)

#### Standard ACB Definition
```cobol
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
```

### Adabas Command Codes

#### Standard Command Usage
```cobol
* S1 - Search by descriptor
MOVE 'S1' TO ACB-COMMAND-CODE
MOVE 00012 TO ACB-FILE-NUMBER
MOVE 100 TO ACB-FORMAT-BUFFER-LEN
MOVE 500 TO ACB-RECORD-BUFFER-LEN
MOVE 30 TO ACB-SEARCH-BUFFER-LEN
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER
                    RECORD-BUFFER
                    SEARCH-BUFFER

* L3 - Read record data
MOVE 'L3' TO ACB-COMMAND-CODE
MOVE SAVED-ISN TO ACB-ISN
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER
                    RECORD-BUFFER

* A1 - Update record
MOVE 'A1' TO ACB-COMMAND-CODE
MOVE SAVED-ISN TO ACB-ISN
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER
                    RECORD-BUFFER
```

### Buffer Management

#### Format Buffer
```cobol
* List fields to retrieve/update
01  FORMAT-BUFFER           PIC X(100) VALUE
    'ACCOUNT-NUMBER,CUSTOMER-NAME,ACCOUNT-TYPE,BALANCE,LAST-TXN-DATE.'.

* Update format - only fields being modified
01  UPDATE-FORMAT-BUFFER    PIC X(50) VALUE
    'BALANCE,LAST-TXN-DATE.'.
```

#### Search Buffer
```cobol
* Format: FIELD-NAME,occurrence,value.
01  SEARCH-BUFFER.
    05  FILLER              PIC X(17) VALUE 'ACCOUNT-NUMBER,1,'.
    05  SB-ACCOUNT-NUMBER   PIC 9(10).
    05  FILLER              PIC X VALUE '.'.
```

#### Record Buffer
```cobol
* Must match format buffer field order and sizes
01  RECORD-BUFFER.
    05  RB-ACCOUNT-NUMBER   PIC 9(10).
    05  RB-CUSTOMER-NAME    PIC X(50).
    05  RB-ACCOUNT-TYPE     PIC X(15).
    05  RB-BALANCE          PIC 9(09)V99.
    05  RB-LAST-TXN-DATE    PIC 9(08).
```

### Response Code Handling

#### Standard Response Code Checks
```cobol
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER
                    RECORD-BUFFER
                    SEARCH-BUFFER

EVALUATE ACB-RESPONSE-CODE
    WHEN 000
        PERFORM 2100-PROCESS-SUCCESSFUL-READ
    WHEN 003
        DISPLAY 'ERROR: Record not found'
        PERFORM 9100-RECORD-NOT-FOUND-ERROR
    WHEN 009
        DISPLAY 'ERROR: File not available'
        PERFORM 9200-FILE-ERROR
    WHEN 017
        DISPLAY 'ERROR: Invalid file number'
        PERFORM 9300-FILE-NUMBER-ERROR
    WHEN 044
        DISPLAY 'ERROR: Record is locked'
        PERFORM 9400-RECORD-LOCKED-ERROR
    WHEN OTHER
        DISPLAY 'ERROR: Adabas Response Code = ' ACB-RESPONSE-CODE
        PERFORM 9900-UNEXPECTED-ERROR
END-EVALUATE
```

### Transaction Control

#### Hold Logic
```cobol
* Hold record for update (prevents concurrent modification)
MOVE 'S1' TO ACB-COMMAND-CODE
MOVE 'HOLD' TO ACB-COMMAND-ID
* ... rest of search logic

* Release hold if update not needed
MOVE 'RI' TO ACB-COMMAND-CODE
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK

* Update record (automatically releases hold)
MOVE 'A1' TO ACB-COMMAND-CODE
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER
                    RECORD-BUFFER
```

#### End of Transaction
```cobol
* Commit changes
MOVE 'ET' TO ACB-COMMAND-CODE
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK

* Backout changes
MOVE 'BT' TO ACB-COMMAND-CODE
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
```

---

## Error Handling

### Standard Error Handling Pattern
```cobol
*----------------------------------------------------------------
* Central error handling routine
*----------------------------------------------------------------
9000-ERROR-HANDLER.
    DISPLAY '=================================================='
    DISPLAY 'PROGRAM ERROR OCCURRED'
    DISPLAY '=================================================='
    DISPLAY 'Program: ' PROGRAM-NAME
    DISPLAY 'Paragraph: ' WS-CURRENT-PARAGRAPH
    DISPLAY 'Error Code: ' WS-ERROR-CODE
    DISPLAY 'Error Message: ' WS-ERROR-MESSAGE
    DISPLAY '=================================================='
    
    PERFORM 9100-WRITE-ERROR-LOG
    PERFORM 9200-CLEANUP-RESOURCES
    
    MOVE 8 TO RETURN-CODE
    STOP RUN.

*----------------------------------------------------------------
9100-WRITE-ERROR-LOG.
*----------------------------------------------------------------
    OPEN EXTEND ERROR-LOG-FILE
    WRITE ERROR-LOG-RECORD FROM WS-ERROR-RECORD
    CLOSE ERROR-LOG-FILE.
```

### File Status Checking
```cobol
OPEN INPUT CUSTOMER-FILE
IF WS-FILE-STATUS NOT = '00'
    MOVE 'CUSTOMER-FILE' TO WS-ERROR-FILE-NAME
    MOVE WS-FILE-STATUS TO WS-ERROR-CODE
    MOVE 'Unable to open file' TO WS-ERROR-MESSAGE
    PERFORM 9000-ERROR-HANDLER
END-IF
```

---

## Documentation Standards

### Program Header Comments
```cobol
      *================================================================
      * PROGRAM: ACCOUNT-UPDATE
      * AUTHOR: John Developer
      * DATE WRITTEN: 2025-11-24
      * PURPOSE: Update customer account balance and transaction date
      *
      * DESCRIPTION:
      *   Interactive program allowing tellers to update account
      *   balances through deposits, withdrawals, or adjustments.
      *   Validates sufficient funds for withdrawals and maintains
      *   transaction audit trail.
      *
      * INPUT:
      *   - User interactive input (account number, transaction type)
      *   - Adabas File 12: CUSTOMER-ACCOUNTS
      *
      * OUTPUT:
      *   - Screen displays (account details, confirmations)
      *   - Updated Adabas records
      *
      * DEPENDENCIES:
      *   - ADABAS module (database interface)
      *   - File 12 must be available and accessible
      *
      * RETURN CODES:
      *   0 - Successful execution
      *   4 - Warning (partial success)
      *   8 - Error (processing failed)
      *
      * SECURITY:
      *   - Requires TELLER or MANAGER authority
      *   - All updates logged for audit
      *================================================================
      * MODIFICATION HISTORY:
      * DATE       AUTHOR      DESCRIPTION
      * ---------- ----------- ---------------------------------------
      * 2025-11-24 J.Developer Initial version
      * 2025-11-25 M.Reviewer  Added insufficient funds validation
      *================================================================
```

### Paragraph Comments
```cobol
      *----------------------------------------------------------------
      * 2100-CALCULATE-NEW-BALANCE
      * PURPOSE: Calculate new balance based on transaction type
      * INPUT: WS-OLD-BALANCE, WS-TRANSACTION-TYPE, WS-AMOUNT
      * OUTPUT: WS-NEW-BALANCE
      * ERRORS: Sets WS-ERROR-FLAG if insufficient funds
      *----------------------------------------------------------------
```

### Inline Comments
```cobol
* Initialize date fields to current system date
ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

* Check for minimum balance requirement (regulation compliance)
IF WS-BALANCE < WS-MINIMUM-BALANCE
    PERFORM 2500-ASSESS-LOW-BALANCE-FEE
END-IF

* Adabas ISN must be saved from search for update operation
MOVE ACB-ISN TO WS-SAVED-ISN
```

---

## Best Practices

### Performance Optimization

1. **Use COMP for Arithmetic**
```cobol
* GOOD: Binary for counters and indexes
01  WS-COUNTER              PIC S9(04) COMP.

* GOOD: Packed decimal for monetary calculations
01  WS-TOTAL-BALANCE        PIC S9(11)V99 COMP-3.
```

2. **Initialize Once, Reuse**
```cobol
* Initialize control block once, modify only needed fields
PERFORM 1000-INITIALIZE-ACB

* In loop, only change what's needed
PERFORM VARYING WS-INDEX FROM 1 BY 1
    UNTIL WS-INDEX > WS-ACCOUNT-COUNT
    MOVE WS-ACCOUNT-NUMBER(WS-INDEX) TO SB-ACCOUNT-NUMBER
    PERFORM 2100-READ-ACCOUNT-RECORD
END-PERFORM
```

3. **Minimize Adabas Calls**
```cobol
* GOOD: Retrieve all needed fields in one call
FORMAT-BUFFER = 'ACCOUNT-NUMBER,CUSTOMER-NAME,BALANCE,DATE.'

* AVOID: Multiple calls for same record
```

### Code Reusability

1. **Create Generic Subprograms**
```cobol
* Instead of: Inline Adabas calls in every program
* Use: Generic database access modules
CALL 'DB-READ' USING WS-ACCOUNT-NUMBER
                     WS-ACCOUNT-RECORD
                     WS-RETURN-CODE
```

2. **Use COPY Books**
```cobol
* Define common structures once
      COPY CUSTOMER-RECORD.
      COPY ADABAS-ACB.
      COPY ERROR-CODES.
```

### Maintainability

1. **Avoid Magic Numbers**
```cobol
* AVOID:
IF WS-ACCOUNT-TYPE = 'C'

* GOOD:
01  WS-ACCOUNT-TYPE         PIC X.
    88  CHECKING-ACCOUNT    VALUE 'C'.
    88  SAVINGS-ACCOUNT     VALUE 'S'.
    88  FIXED-DEPOSIT       VALUE 'F'.

IF CHECKING-ACCOUNT
```

2. **Use Meaningful Variable Names**
```cobol
* AVOID: A, B, X, TEMP
* GOOD: WS-CUSTOMER-BALANCE, WS-TRANSACTION-DATE
```

3. **Keep Paragraphs Focused**
```cobol
* Each paragraph should do ONE thing
* Keep length under 50 lines
* Use PERFORM to chain related operations
```

---

## Prohibited Practices

❌ **Never use:**
- `ALTER` statement
- `GO TO` (except for error handling)
- `EXAMINE` (obsolete, use `INSPECT`)
- Multiple entry points (`ENTRY`)
- `ACCEPT` from system date in production (use proper date routines)

❌ **Avoid:**
- Deeply nested IF statements (use EVALUATE)
- Hard-coded literals (use constants)
- Modifying code using `MOVE` to procedure statements
- Programs over 2000 lines (split into modules)

✅ **Always:**
- Initialize all working storage variables
- Check file status after every I/O operation
- Check Adabas response codes after every call
- Close files before program termination
- Use explicit scope terminators (END-IF, END-PERFORM, etc.)

---

## Code Review Checklist

- [ ] Program header complete with purpose and modification history
- [ ] All variables have meaningful names
- [ ] Level 88 conditions used for readability
- [ ] All file operations check status codes
- [ ] All Adabas calls check response codes
- [ ] Error handling routines implemented
- [ ] No hard-coded literals (use constants)
- [ ] Paragraphs are focused and reasonably sized
- [ ] Code follows structured programming principles
- [ ] Comments explain WHY, not WHAT
- [ ] No obsolete or prohibited language features used
- [ ] Consistent indentation and formatting
- [ ] All IF/PERFORM statements have scope terminators

---

**Document Version:** 1.0  
**Last Updated:** 2025-11-24  
**Maintained By:** Development Team
