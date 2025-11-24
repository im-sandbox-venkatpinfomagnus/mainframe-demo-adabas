package com.mainframe.banking.model;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Account Entity - Modernized from COBOL ACCOUNT-RECORD structure
 * 
 * Original COBOL fields:
 * - ACCOUNT-NUMBER (PIC 9(10)) -> Long accountNumber
 * - CUSTOMER-NAME (PIC X(50)) -> String customerName
 * - ACCOUNT-TYPE (PIC X(15)) -> AccountType enum
 * - BALANCE (PIC 9(09)V99) -> BigDecimal balance
 * - LAST-TXN-DATE (PIC 9(08)) -> LocalDate lastTransactionDate
 */
@Entity
@Table(name = "customer_accounts")
@EntityListeners(AuditingEntityListener.class)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Account {
    
    @Id
    @Column(name = "account_number", nullable = false, unique = true)
    private Long accountNumber;
    
    @Column(name = "customer_name", nullable = false, length = 50)
    private String customerName;
    
    @Enumerated(EnumType.STRING)
    @Column(name = "account_type", nullable = false, length = 15)
    private AccountType accountType;
    
    @Column(name = "balance", nullable = false, precision = 11, scale = 2)
    private BigDecimal balance;
    
    @Column(name = "last_transaction_date", nullable = false)
    private LocalDate lastTransactionDate;
    
    @CreatedDate
    @Column(name = "created_date", nullable = false, updatable = false)
    private LocalDateTime createdDate;
    
    @LastModifiedDate
    @Column(name = "modified_date")
    private LocalDateTime modifiedDate;
    
    /**
     * Account Type Enum - Maps to COBOL ACCOUNT-TYPE field
     * Valid values from seed data: CHECKING, SAVINGS, FIXED-DEPOSIT
     */
    public enum AccountType {
        CHECKING,
        SAVINGS,
        FIXED_DEPOSIT // Renamed from FIXED-DEPOSIT for Java naming convention
    }
}
