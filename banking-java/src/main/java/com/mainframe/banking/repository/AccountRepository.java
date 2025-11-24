package com.mainframe.banking.repository;

import com.mainframe.banking.model.Account;
import com.mainframe.banking.model.Account.AccountType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * Account Repository - Data access layer for Account entity
 * 
 * Replaces COBOL Adabas database operations:
 * - S1 command (Search) -> findByAccountNumber
 * - L3 command (Read) -> findById
 * - A1 command (Update) -> save (automatic)
 */
@Repository
public interface AccountRepository extends JpaRepository<Account, Long> {
    
    /**
     * Find account by account number (Primary key lookup)
     * Replaces COBOL S1 search command with ACCOUNT-NUMBER descriptor
     */
    Optional<Account> findByAccountNumber(Long accountNumber);
    
    /**
     * Find accounts by customer name (Descriptor search)
     * Replaces COBOL S1 search command with CUSTOMER-NAME descriptor
     */
    List<Account> findByCustomerNameContainingIgnoreCase(String customerName);
    
    /**
     * Find accounts by account type
     */
    List<Account> findByAccountType(AccountType accountType);
    
    /**
     * Find accounts with balance greater than specified amount
     */
    List<Account> findByBalanceGreaterThan(BigDecimal amount);
    
    /**
     * Check if account exists by account number
     * Used for validation before operations
     */
    boolean existsByAccountNumber(Long accountNumber);
    
    /**
     * Custom query to get total balance by account type
     * Useful for reporting
     */
    @Query("SELECT a.accountType, SUM(a.balance) FROM Account a GROUP BY a.accountType")
    List<Object[]> getTotalBalanceByType();
    
    /**
     * Find accounts with recent transactions
     */
    @Query("SELECT a FROM Account a WHERE a.lastTransactionDate >= :startDate ORDER BY a.lastTransactionDate DESC")
    List<Account> findRecentTransactions(@Param("startDate") java.time.LocalDate startDate);
}
