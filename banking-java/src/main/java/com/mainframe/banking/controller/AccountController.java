package com.mainframe.banking.controller;

import com.mainframe.banking.dto.AccountResponseDTO;
import com.mainframe.banking.dto.TransactionRequestDTO;
import com.mainframe.banking.dto.TransactionResponseDTO;
import com.mainframe.banking.service.AccountService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Account Controller - REST API Layer
 * 
 * Modernizes COBOL interactive programs to RESTful web services:
 * - ACCOUNT-READ.cbl -> GET /api/accounts/{accountNumber}
 * - ACCOUNT-UPDATE.cbl -> POST /api/accounts/{accountNumber}/transactions
 * 
 * Replaces mainframe terminal I/O with HTTP JSON endpoints
 */
@RestController
@RequestMapping("/api/accounts")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Account Management", description = "Banking account operations API")
public class AccountController {
    
    private final AccountService accountService;
    
    /**
     * GET /api/accounts/{accountNumber}
     * 
     * Read and display account information
     * 
     * Replaces COBOL ACCOUNT-READ.cbl:
     * - Input: WS-INPUT-ACCOUNT (terminal ACCEPT)
     * - Process: 2200-READ-ACCOUNT (Adabas S1 search)
     * - Output: 2300-DISPLAY-ACCOUNT (terminal DISPLAY)
     * 
     * @param accountNumber 10-digit account number
     * @return Account details with formatted balance and date
     */
    @GetMapping("/{accountNumber}")
    @Operation(
        summary = "Get account by number",
        description = "Retrieve customer account information by account number. " +
                     "Replaces COBOL ACCOUNT-READ program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(
            responseCode = "200",
            description = "Account found successfully",
            content = @Content(schema = @Schema(implementation = AccountResponseDTO.class))
        ),
        @ApiResponse(
            responseCode = "404",
            description = "Account not found (COBOL Response Code 003)"
        ),
        @ApiResponse(
            responseCode = "500",
            description = "Internal server error (COBOL Response Code 009)"
        )
    })
    public ResponseEntity<AccountResponseDTO> getAccount(
            @Parameter(description = "10-digit account number", example = "1001234567")
            @PathVariable Long accountNumber) {
        
        log.info("GET /api/accounts/{} - Read account request", accountNumber);
        
        AccountResponseDTO response = accountService.findByAccountNumber(accountNumber);
        
        return ResponseEntity.ok(response);
    }
    
    /**
     * POST /api/accounts/{accountNumber}/transactions
     * 
     * Process account transaction (Deposit, Withdrawal, or Adjustment)
     * 
     * Replaces COBOL ACCOUNT-UPDATE.cbl:
     * - Input: WS-INPUT-ACCOUNT, WS-TRANSACTION-TYPE, WS-AMOUNT (terminal ACCEPT)
     * - Process: 2400-GET-UPDATE-INFO, 2500-UPDATE-ACCOUNT (Adabas A1 update)
     * - Output: 2600-DISPLAY-SUCCESS (terminal DISPLAY)
     * 
     * Transaction Types:
     * - DEPOSIT: Add funds to account
     * - WITHDRAWAL: Deduct funds (with insufficient funds check)
     * - ADJUSTMENT: Set balance to specified amount
     * 
     * @param accountNumber Account to update
     * @param request Transaction details (type and amount)
     * @return Transaction result with before/after balance
     */
    @PostMapping("/{accountNumber}/transactions")
    @Operation(
        summary = "Process account transaction",
        description = "Execute deposit, withdrawal, or balance adjustment. " +
                     "Replaces COBOL ACCOUNT-UPDATE program functionality."
    )
    @ApiResponses(value = {
        @ApiResponse(
            responseCode = "200",
            description = "Transaction completed successfully",
            content = @Content(schema = @Schema(implementation = TransactionResponseDTO.class))
        ),
        @ApiResponse(
            responseCode = "400",
            description = "Invalid request (validation error or insufficient funds)"
        ),
        @ApiResponse(
            responseCode = "404",
            description = "Account not found (COBOL Response Code 003)"
        ),
        @ApiResponse(
            responseCode = "423",
            description = "Record locked (COBOL Response Code 044)"
        )
    })
    public ResponseEntity<TransactionResponseDTO> processTransaction(
            @Parameter(description = "10-digit account number", example = "1001234567")
            @PathVariable Long accountNumber,
            
            @Parameter(description = "Transaction request with type and amount")
            @Valid @RequestBody TransactionRequestDTO request) {
        
        log.info("POST /api/accounts/{}/transactions - {} of {}",
            accountNumber, request.transactionType(), request.amount());
        
        TransactionResponseDTO response = accountService.processTransaction(accountNumber, request);
        
        return ResponseEntity.ok(response);
    }
    
    /**
     * GET /api/accounts/health
     * 
     * Health check endpoint for API availability
     * Useful for monitoring and load balancers
     */
    @GetMapping("/health")
    @Operation(summary = "API health check", description = "Check if the account service is running")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Account Service is running");
    }
}
