package com.mainframe.banking.dto;

import com.mainframe.banking.model.Account.AccountType;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Account Response DTO
 * 
 * Replaces COBOL ACCOUNT-RECORD display output
 * Includes formatted balance for display (like WS-FORMATTED-BALANCE)
 */
public record AccountResponseDTO(
    Long accountNumber,
    String customerName,
    AccountType accountType,
    BigDecimal balance,
    String formattedBalance,  // Format: $ZZZ,ZZZ,ZZ9.99
    LocalDate lastTransactionDate,
    String formattedDate      // Format: YYYY-MM-DD
) {
    /**
     * Format balance for display
     */
    public static String formatBalance(BigDecimal balance) {
        return String.format("$%,.2f", balance);
    }
    
    /**
     * Format date for display
     */
    public static String formatDate(LocalDate date) {
        return date.toString(); // Already in YYYY-MM-DD format
    }
}
