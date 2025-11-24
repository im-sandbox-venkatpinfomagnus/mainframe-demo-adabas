-- Banking Account Service - Seed Data
-- 20 test accounts from COBOL seed-data.txt
-- Migrated from mainframe flat file format to SQL INSERT statements

-- Account Type Distribution:
-- CHECKING: 8 accounts
-- SAVINGS: 7 accounts
-- FIXED_DEPOSIT: 5 accounts
-- Total Balance: $1,091,227.20

-- CHECKING Accounts
INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234567, 'JAMES ANDERSON', 'CHECKING', 15250.75, '2025-11-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234570, 'JENNIFER WILLIAMS', 'CHECKING', 8750.50, '2025-11-21', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234572, 'LINDA DAVIS', 'CHECKING', 12300.00, '2025-11-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234575, 'WILLIAM MOORE', 'CHECKING', 22450.30, '2025-11-22', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234578, 'MARY THOMAS', 'CHECKING', 9850.25, '2025-11-21', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234580, 'BARBARA WHITE', 'CHECKING', 14275.80, '2025-11-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234583, 'THOMAS THOMPSON', 'CHECKING', 18920.45, '2025-11-22', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234586, 'KAREN ROBINSON', 'CHECKING', 11680.90, '2025-11-21', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- SAVINGS Accounts
INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234568, 'MARIA GARCIA', 'SAVINGS', 45000.00, '2025-11-19', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234571, 'MICHAEL BROWN', 'SAVINGS', 32500.25, '2025-11-18', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234574, 'SUSAN WILSON', 'SAVINGS', 58900.75, '2025-11-17', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234576, 'PATRICIA TAYLOR', 'SAVINGS', 41200.00, '2025-11-16', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234579, 'CHARLES JACKSON', 'SAVINGS', 67300.50, '2025-11-15', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234582, 'SARAH MARTIN', 'SAVINGS', 38750.00, '2025-11-19', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234584, 'NANCY GARCIA', 'SAVINGS', 52100.75, '2025-11-14', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

-- FIXED_DEPOSIT Accounts
INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234569, 'ROBERT JOHNSON', 'FIXED_DEPOSIT', 100000.00, '2025-10-15', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234573, 'DAVID MILLER', 'FIXED_DEPOSIT', 75000.00, '2025-11-01', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234577, 'RICHARD ANDERSON', 'FIXED_DEPOSIT', 150000.00, '2025-10-20', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234581, 'JOSEPH HARRIS', 'FIXED_DEPOSIT', 200000.00, '2025-11-05', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO customer_accounts (account_number, customer_name, account_type, balance, last_transaction_date, created_date, modified_date)
VALUES (1001234585, 'DANIEL LEE', 'FIXED_DEPOSIT', 125000.00, '2025-11-10', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
