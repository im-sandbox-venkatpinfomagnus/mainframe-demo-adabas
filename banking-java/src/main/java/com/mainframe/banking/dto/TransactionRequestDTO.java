package com.mainframe.banking.dto;

import com.mainframe.banking.model.Account.AccountType;
import jakarta.validation.constraints.*;

import java.math.BigDecimal;

/**
 * Transaction Request DTO
 * 
 * Replaces COBOL working variables:
 * - WS-TRANSACTION-TYPE (PIC X) -> TransactionType enum
 * - WS-AMOUNT (PIC 9(09)V99) -> BigDecimal amount
 * 
 * Used for ACCOUNT-UPDATE operations (Deposit, Withdrawal, Adjustment)
 */
public record TransactionRequestDTO(
    
    @NotNull(message = "Transaction type is required")
    TransactionType transactionType,
    
    @NotNull(message = "Amount is required")
    @DecimalMin(value = "0.01", message = "Amount must be greater than 0")
    @DecimalMax(value = "999999999.99", message = "Amount exceeds maximum limit")
    @Digits(integer = 9, fraction = 2, message = "Amount must have at most 9 digits and 2 decimal places")
    BigDecimal amount
) {
    /**
     * Transaction Type Enum
     * Maps to COBOL WS-TRANSACTION-TYPE with 88-level conditions:
     * - D (DEPOSIT) -> DEPOSIT
     * - W (WITHDRAWAL) -> WITHDRAWAL
     * - A (BALANCE-ADJUSTMENT) -> ADJUSTMENT
     */
    public enum TransactionType {
        DEPOSIT,     // D in COBOL
        WITHDRAWAL,  // W in COBOL
        ADJUSTMENT   // A in COBOL
    }
}
