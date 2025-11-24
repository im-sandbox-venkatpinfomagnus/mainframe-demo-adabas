package com.mainframe.banking.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Transaction Response DTO
 * 
 * Replaces COBOL success display section (2600-DISPLAY-SUCCESS)
 * Shows before/after balance and transaction details
 */
public record TransactionResponseDTO(
    Long accountNumber,
    String customerName,
    BigDecimal oldBalance,
    BigDecimal newBalance,
    BigDecimal transactionAmount,
    String transactionType,
    LocalDateTime transactionDate,
    String message
) {
}
