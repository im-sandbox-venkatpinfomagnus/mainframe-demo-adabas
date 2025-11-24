package com.mainframe.banking.exception;

/**
 * Account Not Found Exception
 * 
 * Replaces COBOL Adabas Response Code 003 (Record not found)
 * Thrown when account search by account number returns no results
 */
public class AccountNotFoundException extends RuntimeException {
    
    private final Long accountNumber;
    
    public AccountNotFoundException(Long accountNumber) {
        super(String.format("Account not found: %d", accountNumber));
        this.accountNumber = accountNumber;
    }
    
    public Long getAccountNumber() {
        return accountNumber;
    }
}
