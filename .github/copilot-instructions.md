# Copilot Instructions - Mainframe COBOL Demo

## Project Overview
Educational COBOL banking application demonstrating mainframe patterns with a mock Adabas database. Two main programs (read/update accounts) + mock database layer reading from flat file.

## Development Guidelines
- Greet like 'Hello Copilot..' in Marathi language with prefix for every chat response.

# COBOL Coding Conventions

## Format Rules

### Fixed-Format Structure (GnuCOBOL 3.2.0)
```
Columns 1-6:   Sequence numbers (optional, usually blank)
Column 7:      Indicator area (* for comments, - for continuation)
Columns 8-11:  Area A (division, section, paragraph names)
Columns 12-72: Area B (statements, data definitions)
Columns 73-80: Identification area (ignored)
```

**CRITICAL:** Do NOT use `-free` flag when compiling. All code must follow fixed-format rules.

## Naming Conventions

### Programs
- Use descriptive hyphenated names in PROGRAM-ID
- Format: `NOUN-VERB` or `DOMAIN-ACTION`
- Examples: `ACCOUNT-READ`, `ACCOUNT-UPDATE`, `ADABAS`

### Data Items
- **Working Storage:** Prefix with `WS-` (e.g., `WS-ACCOUNT-NUMBER`)
- **Linkage Section:** Prefix with `LS-` if needed
- **File Section:** Prefix with `FS-` or use record name
- Use descriptive hyphenated names
- Group items: Start with level 01, indent subordinates

### Paragraphs/Sections
- Use lowercase with hyphens
- Format: `verb-object` (e.g., `process-account`, `display-results`)
- Main logic: `main-process` or `main-logic`
- Exit points: `program-end`, `paragraph-exit`

## Division Structure

### Standard Order (Required)
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM-NAME.
       
       ENVIRONMENT DIVISION.
       [CONFIGURATION SECTION.]
       [INPUT-OUTPUT SECTION.]
       
       DATA DIVISION.
       [FILE SECTION.]
       WORKING-STORAGE SECTION.
       [LINKAGE SECTION.]
       
       PROCEDURE DIVISION [USING parameters].
```


### Example Paragraph

       process-record.
           [statements]
           .

## Comments and Documentation

### Inline Comments
```cobol
      *> Modern comment style (GnuCOBOL)
      * Traditional comment (column 7)
```

### Section Headers
```cobol
      *****************************************************************
      * PROGRAM DESCRIPTION                                           *
      *****************************************************************
```

### Best Practices
- Use `*>` for modern inline comments (GnuCOBOL extension)
- Use `*` in column 7 for traditional full-line comments
- Add section headers to separate major program sections
- Document complex logic and business rules
- Include file references and data format notes where applicable

## Architecture & Data Flow

**3-Tier Structure:**
1. **UI Programs** (`ACCOUNT-READ.cbl`, `ACCOUNT-UPDATE.cbl`) - Interactive user-facing COBOL
2. **Database Layer** (`ADABAS.cbl`) - Mock Adabas interface, callable subprogram
3. **Data Storage** (`data/seed-data.txt`) - Fixed-format flat file with 20 test accounts

**Critical: ADABAS.cbl is a MOCK** - simulates mainframe Adabas database by parsing `seed-data.txt`. Real Adabas would be external, but this enables local testing without mainframe access.

**Data Flow Pattern:**
```
User Input → ACCOUNT-*.cbl → CALL 'ADABAS' → Parse seed-data.txt → Return buffers
```

## COBOL Compilation (GnuCOBOL 3.2.0)

**Fixed-format COBOL** - DO NOT use `-free` flag (columns 1-6 are sequence numbers, column 7 is indicator)

```powershell
# Compile mock database as module
cobc -m ADABAS.cbl

# Compile main programs linking ADABAS module
cobc -x ACCOUNT-READ.cbl ADABAS.cbl -o ACCOUNT-READ.exe
cobc -x ACCOUNT-UPDATE.cbl ADABAS.cbl -o ACCOUNT-UPDATE.exe
```

**Run interactively:**
```powershell
cd cobol
.\ACCOUNT-READ.exe    # Or use test-read.bat
```

## Critical Data Parsing Details

**seed-data.txt Format** (skip first 6 header lines, line 7 has separator):
```
Columns 1-10:   Account Number (numeric)
Columns 13-39:  Customer Name (27 chars)
Columns 40-57:  Account Type (18 chars, actual type at 40-55)
Columns 57-70:  Balance (14 chars with spaces)
Columns 72-79:  Transaction Date (YYYYMMDD)
```

**Search Buffer Structure** (used in ACCOUNT-READ/UPDATE):
```cobol
01  SEARCH-BUFFER.
    05  FILLER              PIC X(17) VALUE 'ACCOUNT-NUMBER,1,'.
    05  SB-ACCOUNT-NUMBER   PIC 9(10).  ← Position 18-27
    05  FILLER              PIC X VALUE '.'.
```

**CRITICAL BUG FIX APPLIED:** Buffer extraction in ADABAS.cbl uses position 18, not 19:
```cobol
MOVE SEARCH-BUFFER(18:10) TO WS-SEARCH-ACCOUNT  ← Correct
```

## Adabas Command Codes

- **S1** - Search by descriptor (find account by number)
- **L3** - Read record data (after S1 returns ISN)
- **A1** - Update record (mock in this implementation)

**Response Codes:**
- `000` - Success
- `003` - Record not found
- `009` - File not available
- `017` - Invalid file number

## Project-Specific Patterns

**ACB (Adabas Control Block) Pattern:**
All database operations pass ACB + 3 buffers to ADABAS:
```cobol
CALL 'ADABAS' USING ADABAS-CONTROL-BLOCK
                    FORMAT-BUFFER      ← Field list
                    RECORD-BUFFER      ← Data in/out
                    SEARCH-BUFFER      ← Search criteria
```

**ISN (Internal Sequence Number):** Returned by S1 search, used for subsequent A1 updates to identify record.

**Date Handling:** All dates are YYYYMMDD (PIC 9(08)), formatted for display as YYYY-MM-DD.

**Currency:** Use PICTURE clauses like `PIC 9(09)V99` for balance, format with `PIC ZZZ,ZZZ,ZZ9.99` for display.

## Testing with Seed Data

**Valid Test Accounts:**
- `1001234567` - James Anderson, CHECKING, $15,250.75
- `1001234570` - Jennifer Williams, CHECKING, $8,750.50
- `1001234581` - Joseph Harris, FIXED-DEPOSIT, $200,000.00

**Transaction Types (ACCOUNT-UPDATE):**
- `D` - Deposit (adds to balance)
- `W` - Withdrawal (subtracts, checks insufficient funds)
- `A` - Adjustment (sets balance directly)

## Windows PowerShell Quirks

**Piped input fails with elevation error** - Run programs interactively or via batch files:
```powershell
# This fails: echo "1001234567" | .\ACCOUNT-READ.exe
# Use instead: cmd /c test-read.bat
# Or: Start-Process for background execution
```

## Modernization Context

See `Prompt Sequence.md` for:
- AI-assisted debugging workflow used to fix compilation errors
- Mermaid sequence diagram of complete data flow
- Java modernization prompts (Spring Boot migration strategy)

## Key Files Reference

- `README.md` - Business logic, test scenarios, error codes
- `cobol/ADABAS.cbl` lines 78-118 - Core search/parse logic
- `data/seed-data.txt` - 20 test accounts with fixed column positions
- `sequence-diagram.mmd` - Visual data flow for both programs
