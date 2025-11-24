-- Banking Account Service Database Schema
-- Modernized from COBOL ACCOUNT-RECORD and Adabas FDT

-- Drop table if exists (for clean restart)
DROP TABLE IF EXISTS customer_accounts;

-- Create customer_accounts table
-- Maps COBOL PIC clauses to SQL types:
-- PIC 9(10) -> BIGINT (account_number)
-- PIC X(50) -> VARCHAR(50) (customer_name)
-- PIC X(15) -> VARCHAR(15) (account_type)
-- PIC 9(09)V99 -> DECIMAL(11,2) (balance)
-- PIC 9(08) -> DATE (last_transaction_date)

CREATE TABLE customer_accounts (
    account_number BIGINT PRIMARY KEY,
    customer_name VARCHAR(50) NOT NULL,
    account_type VARCHAR(15) NOT NULL CHECK (account_type IN ('CHECKING', 'SAVINGS', 'FIXED_DEPOSIT')),
    balance DECIMAL(11, 2) NOT NULL CHECK (balance >= 0),
    last_transaction_date DATE NOT NULL,
    created_date TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    modified_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for performance
CREATE INDEX idx_customer_name ON customer_accounts(customer_name);
CREATE INDEX idx_account_type ON customer_accounts(account_type);
CREATE INDEX idx_last_txn_date ON customer_accounts(last_transaction_date);

-- Comments for documentation
COMMENT ON TABLE customer_accounts IS 'Customer banking accounts - modernized from COBOL Adabas file';
COMMENT ON COLUMN customer_accounts.account_number IS 'Unique 10-digit account identifier (COBOL: PIC 9(10))';
COMMENT ON COLUMN customer_accounts.customer_name IS 'Customer full name (COBOL: PIC X(50))';
COMMENT ON COLUMN customer_accounts.account_type IS 'Account type: CHECKING, SAVINGS, or FIXED_DEPOSIT (COBOL: PIC X(15))';
COMMENT ON COLUMN customer_accounts.balance IS 'Account balance (COBOL: PIC 9(09)V99)';
COMMENT ON COLUMN customer_accounts.last_transaction_date IS 'Last transaction date (COBOL: PIC 9(08) YYYYMMDD)';
