package com.mainframe.banking.service;

import com.mainframe.banking.dto.AccountResponseDTO;
import com.mainframe.banking.dto.TransactionRequestDTO;
import com.mainframe.banking.dto.TransactionResponseDTO;
import com.mainframe.banking.exception.AccountNotFoundException;
import com.mainframe.banking.exception.InsufficientFundsException;
import com.mainframe.banking.model.Account;
import com.mainframe.banking.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Account Service - Business Logic Layer
 * 
 * Replicates COBOL program logic from:
 * - ACCOUNT-READ.cbl (2200-READ-ACCOUNT, 2300-DISPLAY-ACCOUNT)
 * - ACCOUNT-UPDATE.cbl (2400-GET-UPDATE-INFO, 2500-UPDATE-ACCOUNT, 2600-DISPLAY-SUCCESS)
 * 
 * Implements business rules and validations from mainframe application
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AccountService {
    
    private final AccountRepository accountRepository;
    
    /**
     * Find account by account number
     * 
     * Replaces COBOL: 2200-READ-ACCOUNT paragraph
     * - Performs S1 search by ACCOUNT-NUMBER descriptor
     * - Returns account details or throws exception if not found
     * 
     * @param accountNumber 10-digit account number
     * @return AccountResponseDTO with formatted display values
     * @throws AccountNotFoundException if account not found (Response Code 003)
     */
    @Transactional(readOnly = true)
    public AccountResponseDTO findByAccountNumber(Long accountNumber) {
        log.info("Searching for account: {}", accountNumber);
        
        Account account = accountRepository.findByAccountNumber(accountNumber)
            .orElseThrow(() -> new AccountNotFoundException(accountNumber));
        
        log.info("Account found: {} - {}", account.getAccountNumber(), account.getCustomerName());
        
        return mapToResponseDTO(account);
    }
    
    /**
     * Process transaction (Deposit, Withdrawal, or Adjustment)
     * 
     * Replaces COBOL: 2400-GET-UPDATE-INFO and 2500-UPDATE-ACCOUNT paragraphs
     * 
     * Business Rules (from COBOL EVALUATE TRUE):
     * 1. DEPOSIT: Add amount to current balance
     * 2. WITHDRAWAL: Subtract amount, check insufficient funds
     * 3. ADJUSTMENT: Set balance to specified amount
     * 
     * @param accountNumber Account to update
     * @param request Transaction details
     * @return TransactionResponseDTO with before/after balance
     * @throws AccountNotFoundException if account not found
     * @throws InsufficientFundsException if withdrawal exceeds balance
     */
    @Transactional
    public TransactionResponseDTO processTransaction(Long accountNumber, TransactionRequestDTO request) {
        log.info("Processing {} for account: {}, amount: {}",
            request.transactionType(), accountNumber, request.amount());
        
        // Read account (S1 + L3 in COBOL)
        Account account = accountRepository.findByAccountNumber(accountNumber)
            .orElseThrow(() -> new AccountNotFoundException(accountNumber));
        
        BigDecimal oldBalance = account.getBalance();
        BigDecimal newBalance;
        String transactionDescription;
        
        // Process transaction based on type (COBOL EVALUATE TRUE)
        switch (request.transactionType()) {
            case DEPOSIT -> {
                // COBOL: WHEN DEPOSIT
                // COMPUTE WS-NEW-BALANCE = WS-OLD-BALANCE + WS-AMOUNT
                newBalance = oldBalance.add(request.amount());
                transactionDescription = "Deposit";
                log.info("Deposit: {} + {} = {}", oldBalance, request.amount(), newBalance);
            }
            case WITHDRAWAL -> {
                // COBOL: WHEN WITHDRAWAL
                // IF WS-AMOUNT > WS-OLD-BALANCE
                //     DISPLAY 'ERROR: Insufficient funds'
                if (request.amount().compareTo(oldBalance) > 0) {
                    log.warn("Insufficient funds: requested={}, available={}", request.amount(), oldBalance);
                    throw new InsufficientFundsException(accountNumber, request.amount(), oldBalance);
                }
                // COMPUTE WS-NEW-BALANCE = WS-OLD-BALANCE - WS-AMOUNT
                newBalance = oldBalance.subtract(request.amount());
                transactionDescription = "Withdrawal";
                log.info("Withdrawal: {} - {} = {}", oldBalance, request.amount(), newBalance);
            }
            case ADJUSTMENT -> {
                // COBOL: WHEN BALANCE-ADJUSTMENT
                // MOVE WS-AMOUNT TO WS-NEW-BALANCE
                newBalance = request.amount();
                transactionDescription = "Balance Adjustment";
                log.info("Adjustment: {} -> {}", oldBalance, newBalance);
            }
            default -> {
                // COBOL: WHEN OTHER
                log.error("Invalid transaction type: {}", request.transactionType());
                throw new IllegalArgumentException("Invalid transaction type");
            }
        }
        
        // Update account (A1 command in COBOL)
        account.setBalance(newBalance);
        account.setLastTransactionDate(LocalDate.now());
        accountRepository.save(account);
        
        log.info("Transaction completed successfully for account: {}", accountNumber);
        
        // Return response (COBOL: 2600-DISPLAY-SUCCESS)
        return new TransactionResponseDTO(
            account.getAccountNumber(),
            account.getCustomerName(),
            oldBalance,
            newBalance,
            request.amount(),
            transactionDescription,
            LocalDateTime.now(),
            "Transaction completed successfully"
        );
    }
    
    /**
     * Map Account entity to Response DTO
     * 
     * Replaces COBOL: 2300-DISPLAY-ACCOUNT paragraph
     * - Formats balance (like WS-FORMATTED-BALANCE PIC ZZZ,ZZZ,ZZ9.99)
     * - Formats date (like WS-FORMATTED-DATE YYYY-MM-DD)
     */
    private AccountResponseDTO mapToResponseDTO(Account account) {
        return new AccountResponseDTO(
            account.getAccountNumber(),
            account.getCustomerName(),
            account.getAccountType(),
            account.getBalance(),
            AccountResponseDTO.formatBalance(account.getBalance()),
            account.getLastTransactionDate(),
            AccountResponseDTO.formatDate(account.getLastTransactionDate())
        );
    }
}
