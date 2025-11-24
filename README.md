# Customer Account Management System - Mainframe Demo

## Overview
This demo showcases a complete Adabas database application with COBOL programs for customer account management in a banking environment.

## Components

### 1. Database Structure
**File:** `adabas/CUSTOMER-ACCOUNTS.fdt`

The Adabas file contains the following fields:
- **ACCOUNT-NUMBER** (N10) - Unique account identifier (Descriptor, Unique)
- **CUSTOMER-NAME** (A50) - Customer full name (Descriptor)
- **ACCOUNT-TYPE** (A15) - Account type: SAVINGS, CHECKING, or FIXED-DEPOSIT
- **BALANCE** (P11.2) - Account balance (Packed Decimal)
- **LAST-TXN-DATE** (D8) - Last transaction date (YYYYMMDD format)

### 2. Seed Data
**File:** `data/seed-data.txt`

Contains 20 customer accounts with realistic data:
- 8 CHECKING accounts
- 7 SAVINGS accounts
- 5 FIXED-DEPOSIT accounts
- Total balance: $1,091,227.20

### 3. Programs

#### LOAD-SEED-DATA.NSP (Natural)
Loads the 20 seed records into the Adabas database.

**Usage:**
```
LOGON to Natural
STOW LOAD-SEED-DATA
RUN LOAD-SEED-DATA
```

#### ACCOUNT-READ.cbl (COBOL)
Interactive program to read and display account information.

**Features:**
- Search account by account number
- Display complete account details
- Formatted balance and date output
- Error handling for missing accounts
- Multiple inquiry support

**Usage:**
```
COMPILE: COBOL ACCOUNT-READ
RUN: ACCOUNT-READ
```

**Sample Flow:**
1. Enter account number (e.g., 1001234567)
2. View account details
3. Option to read another account

#### ACCOUNT-UPDATE.cbl (COBOL)
Interactive program to update account balance and transaction date.

**Features:**
- Three transaction types:
  - **D** - Deposit (add funds)
  - **W** - Withdrawal (deduct funds with insufficient funds check)
  - **A** - Balance Adjustment (set new balance)
- Display before and after balance
- Confirmation prompt before update
- Automatic transaction date update
- Comprehensive error handling
- Record locking support

**Usage:**
```
COMPILE: COBOL ACCOUNT-UPDATE
RUN: ACCOUNT-UPDATE
```

**Sample Flow:**
1. Enter account number
2. View current account information
3. Select transaction type (D/W/A)
4. Enter amount
5. Confirm update
6. View success confirmation

## Setup Instructions

### Step 1: Create Adabas File
```
ADADEF CREATE FILE 12 WITH FDT FROM CUSTOMER-ACCOUNTS.fdt
```

### Step 2: Load Seed Data
```
Run LOAD-SEED-DATA.NSP program
```

### Step 3: Compile COBOL Programs
```
COBC ACCOUNT-READ.cbl -o ACCOUNT-READ
COBC ACCOUNT-UPDATE.cbl -o ACCOUNT-UPDATE
```

### Step 4: Test Programs
```
Execute ACCOUNT-READ
Execute ACCOUNT-UPDATE
```

## Sample Test Scenarios

### Scenario 1: Read Account
- Run ACCOUNT-READ
- Enter: 1001234567
- Expected: Display James Anderson's checking account with $15,250.75 balance

### Scenario 2: Deposit
- Run ACCOUNT-UPDATE
- Enter: 1001234568 (Maria Garcia - SAVINGS)
- Select: D (Deposit)
- Amount: 5000.00
- Expected: Balance updated from $45,000.00 to $50,000.00

### Scenario 3: Withdrawal
- Run ACCOUNT-UPDATE
- Enter: 1001234570 (Jennifer Williams - CHECKING)
- Select: W (Withdrawal)
- Amount: 2000.00
- Expected: Balance updated from $8,750.50 to $6,750.50

### Scenario 4: Insufficient Funds
- Run ACCOUNT-UPDATE
- Enter: 1001234578 (Mary Thomas - CHECKING, balance $9,850.25)
- Select: W (Withdrawal)
- Amount: 15000.00
- Expected: Error message - Insufficient funds

### Scenario 5: Balance Adjustment
- Run ACCOUNT-UPDATE
- Enter: 1001234580 (Barbara White)
- Select: A (Adjustment)
- Amount: 15000.00
- Expected: Balance adjusted to $15,000.00

## Error Handling

Both programs handle the following Adabas response codes:
- **000** - Success
- **003** - Record not found
- **009** - File not available
- **017** - Invalid file number
- **044** - Record locked (UPDATE only)
- **145** - Update not allowed (UPDATE only)

## Technical Notes

### Adabas Commands Used:
- **S1** - Search (find record by descriptor)
- **L3** - Read (read record data)
- **A1** - Update (modify existing record)

### COBOL Features Demonstrated:
- Adabas interface using CALL statements
- Control Block (ACB) management
- Format, Search, and Record buffers
- Interactive user input/output
- Data validation
- Error handling
- Formatted numeric display
- Date formatting

## File Structure
```
mainframe-demo/
├── adabas/
│   └── CUSTOMER-ACCOUNTS.fdt
├── data/
│   └── seed-data.txt
├── natural/
│   └── LOAD-SEED-DATA.NSP
├── cobol/
│   ├── ACCOUNT-READ.cbl
│   └── ACCOUNT-UPDATE.cbl
└── README.md
```

## Demo Presentation Tips

1. **Start with the database structure** - Show the FDT and explain field types
2. **Show the seed data** - Highlight diverse account types and balances
3. **Demonstrate READ operations** - Query multiple accounts
4. **Demonstrate UPDATE operations** - Show all three transaction types
5. **Show error handling** - Attempt withdrawal with insufficient funds
6. **Highlight mainframe best practices** - Error codes, validation, confirmation

## Future Enhancements

Potential additions for extended demos:
- CREATE operation (open new account)
- DELETE operation (close account)
- List all accounts by type
- Transaction history logging
- Interest calculation
- Account statement generation
- Multi-record processing (batch updates)

---
**Demo Version:** 1.0  
**Date:** November 22, 2025  
**Target Audience:** Mainframe Developers (Adabas/COBOL)
