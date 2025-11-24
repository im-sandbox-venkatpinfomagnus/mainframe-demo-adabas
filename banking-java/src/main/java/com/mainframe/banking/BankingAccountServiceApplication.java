package com.mainframe.banking;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Banking Account Service - Spring Boot Application
 * 
 * Modernized COBOL mainframe banking application
 * 
 * Original COBOL Programs:
 * - ACCOUNT-READ.cbl (Interactive account inquiry)
 * - ACCOUNT-UPDATE.cbl (Transaction processing)
 * - ADABAS.cbl (Mock database interface)
 * 
 * Spring Boot Architecture:
 * - Controller: REST API endpoints (replaces terminal I/O)
 * - Service: Business logic (replaces COBOL PROCEDURE DIVISION)
 * - Repository: Data access (replaces Adabas calls)
 * - Entity: Domain model (replaces COBOL ACCOUNT-RECORD)
 * 
 * Database: 20 test accounts from seed-data.txt
 * 
 * API Documentation: http://localhost:8080/swagger-ui.html
 * H2 Console: http://localhost:8080/h2-console
 */
@SpringBootApplication
public class BankingAccountServiceApplication {
    
    public static void main(String[] args) {
        SpringApplication.run(BankingAccountServiceApplication.class, args);
    }
}
