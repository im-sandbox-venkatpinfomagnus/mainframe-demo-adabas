package com.mainframe.banking.exception;

import java.math.BigDecimal;

/**
 * Insufficient Funds Exception
 * 
 * Replaces COBOL insufficient funds check in ACCOUNT-UPDATE:
 * IF WS-AMOUNT > WS-OLD-BALANCE
 *     DISPLAY 'ERROR: Insufficient funds'
 * 
 * Thrown during withdrawal operations when balance is insufficient
 */
public class InsufficientFundsException extends RuntimeException {
    
    private final Long accountNumber;
    private final BigDecimal requestedAmount;
    private final BigDecimal availableBalance;
    
    public InsufficientFundsException(Long accountNumber, BigDecimal requestedAmount, BigDecimal availableBalance) {
        super(String.format("Insufficient funds in account %d. Requested: $%,.2f, Available: $%,.2f",
            accountNumber, requestedAmount, availableBalance));
        this.accountNumber = accountNumber;
        this.requestedAmount = requestedAmount;
        this.availableBalance = availableBalance;
    }
    
    public Long getAccountNumber() {
        return accountNumber;
    }
    
    public BigDecimal getRequestedAmount() {
        return requestedAmount;
    }
    
    public BigDecimal getAvailableBalance() {
        return availableBalance;
    }
}
