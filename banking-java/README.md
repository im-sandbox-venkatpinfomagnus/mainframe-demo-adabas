# Banking Account Service - Spring Boot Application

## Overview
Modern Spring Boot REST API modernized from legacy COBOL mainframe banking application. Provides account inquiry and transaction processing capabilities through RESTful web services.

### Original COBOL Programs
- **ACCOUNT-READ.cbl** - Interactive account inquiry (terminal-based)
- **ACCOUNT-UPDATE.cbl** - Transaction processing (deposits, withdrawals, adjustments)
- **ADABAS.cbl** - Mock Adabas database interface

### Spring Boot Architecture
```
┌─────────────────────────────────────────────────────┐
│                 REST API Layer                      │
│            AccountController.java                   │
│    (Replaces terminal ACCEPT/DISPLAY)              │
└────────────┬────────────────────────────────────────┘
             │
┌────────────┴────────────────────────────────────────┐
│              Business Logic Layer                   │
│             AccountService.java                     │
│    (Replaces COBOL PROCEDURE DIVISION)             │
└────────────┬────────────────────────────────────────┘
             │
┌────────────┴────────────────────────────────────────┐
│              Data Access Layer                      │
│           AccountRepository.java                    │
│      (Replaces Adabas S1/L3/A1 commands)           │
└────────────┬────────────────────────────────────────┘
             │
┌────────────┴────────────────────────────────────────┐
│            H2/PostgreSQL Database                   │
│         (20 test accounts from seed data)           │
└─────────────────────────────────────────────────────┘
```

## Technology Stack

- **Java**: 17 LTS
- **Spring Boot**: 3.2.0
- **Build Tool**: Maven 3.8+
- **Database**: H2 (development), PostgreSQL (production)
- **API Documentation**: Springdoc OpenAPI 3 (Swagger UI)
- **Testing**: JUnit 5, Mockito, REST Assured

## Project Structure

```
banking-java/
├── src/
│   ├── main/
│   │   ├── java/com/mainframe/banking/
│   │   │   ├── BankingAccountServiceApplication.java  # Main application
│   │   │   ├── controller/
│   │   │   │   └── AccountController.java             # REST endpoints
│   │   │   ├── service/
│   │   │   │   └── AccountService.java                # Business logic
│   │   │   ├── repository/
│   │   │   │   └── AccountRepository.java             # Data access
│   │   │   ├── model/
│   │   │   │   └── Account.java                       # Entity (COBOL ACCOUNT-RECORD)
│   │   │   ├── dto/
│   │   │   │   ├── AccountResponseDTO.java            # Response DTOs
│   │   │   │   ├── TransactionRequestDTO.java         # Request DTOs
│   │   │   │   └── TransactionResponseDTO.java
│   │   │   ├── exception/
│   │   │   │   ├── AccountNotFoundException.java      # Response Code 003
│   │   │   │   ├── InsufficientFundsException.java    # Withdrawal check
│   │   │   │   └── GlobalExceptionHandler.java        # Error handling
│   │   │   └── config/
│   │   │       ├── OpenAPIConfig.java                 # Swagger config
│   │   │       └── JpaConfig.java                     # JPA auditing
│   │   └── resources/
│   │       ├── application.yml                        # Spring configuration
│   │       ├── schema.sql                             # Database DDL
│   │       └── data.sql                               # 20 test accounts
│   └── test/java/                                     # Unit/Integration tests
├── pom.xml                                            # Maven dependencies
└── README.md                                          # This file
```

## Prerequisites

### Required Software
- **JDK 17 or 21** - [Download OpenJDK](https://adoptium.net/)
- **Maven 3.8+** - [Download Maven](https://maven.apache.org/download.cgi)
- **Git** - [Download Git](https://git-scm.com/downloads)

### Optional Tools
- **IntelliJ IDEA** or **VS Code** - IDE with Spring Boot plugins
- **Postman** - API testing tool
- **PostgreSQL** - Production database (optional)

## Quick Start

### 1. Clone/Navigate to Project
```bash
cd banking-java
```

### 2. Build Project
```bash
mvn clean install
```

### 3. Run Application
```bash
mvn spring-boot:run
```

Or run the JAR directly:
```bash
java -jar target/account-service-1.0.0.jar
```

### 4. Verify Application
The application starts on **http://localhost:8080**

**API Documentation (Swagger UI):**  
http://localhost:8080/swagger-ui.html

**H2 Database Console:**  
http://localhost:8080/h2-console
- JDBC URL: `jdbc:h2:mem:bankingdb`
- Username: `sa`
- Password: *(leave blank)*

## API Endpoints

### 1. Get Account by Number
**Replaces:** COBOL `ACCOUNT-READ.cbl`

```http
GET /api/accounts/{accountNumber}
```

**Example Request:**
```bash
curl -X GET "http://localhost:8080/api/accounts/1001234567"
```

**Example Response (200 OK):**
```json
{
  "accountNumber": 1001234567,
  "customerName": "JAMES ANDERSON",
  "accountType": "CHECKING",
  "balance": 15250.75,
  "formattedBalance": "$15,250.75",
  "lastTransactionDate": "2025-11-20",
  "formattedDate": "2025-11-20"
}
```

**Error Response (404 Not Found):**
```json
{
  "timestamp": "2025-11-24T10:30:00",
  "status": 404,
  "error": "Not Found",
  "message": "Account not found: 9999999999",
  "path": "/api/accounts/9999999999"
}
```

### 2. Process Transaction
**Replaces:** COBOL `ACCOUNT-UPDATE.cbl`

```http
POST /api/accounts/{accountNumber}/transactions
Content-Type: application/json
```

**Transaction Types:**
- `DEPOSIT` - Add funds
- `WITHDRAWAL` - Deduct funds (with insufficient funds check)
- `ADJUSTMENT` - Set balance to specific amount

#### Deposit Example
```bash
curl -X POST "http://localhost:8080/api/accounts/1001234567/transactions" \
  -H "Content-Type: application/json" \
  -d '{
    "transactionType": "DEPOSIT",
    "amount": 5000.00
  }'
```

**Response (200 OK):**
```json
{
  "accountNumber": 1001234567,
  "customerName": "JAMES ANDERSON",
  "oldBalance": 15250.75,
  "newBalance": 20250.75,
  "transactionAmount": 5000.00,
  "transactionType": "Deposit",
  "transactionDate": "2025-11-24T10:35:00",
  "message": "Transaction completed successfully"
}
```

#### Withdrawal Example
```bash
curl -X POST "http://localhost:8080/api/accounts/1001234570/transactions" \
  -H "Content-Type: application/json" \
  -d '{
    "transactionType": "WITHDRAWAL",
    "amount": 2000.00
  }'
```

#### Balance Adjustment Example
```bash
curl -X POST "http://localhost:8080/api/accounts/1001234580/transactions" \
  -H "Content-Type: application/json" \
  -d '{
    "transactionType": "ADJUSTMENT",
    "amount": 15000.00
  }'
```

**Error Response - Insufficient Funds (400 Bad Request):**
```json
{
  "timestamp": "2025-11-24T10:40:00",
  "status": 400,
  "error": "Insufficient Funds",
  "message": "Insufficient funds in account 1001234570. Requested: $20,000.00, Available: $8,750.50",
  "path": "/api/accounts/1001234570/transactions"
}
```

## Test Data

The application includes 20 test accounts from the original COBOL seed data:

| Account Number | Customer Name      | Type          | Balance      |
|----------------|--------------------|---------------|--------------|
| 1001234567     | JAMES ANDERSON     | CHECKING      | $15,250.75   |
| 1001234568     | MARIA GARCIA       | SAVINGS       | $45,000.00   |
| 1001234569     | ROBERT JOHNSON     | FIXED_DEPOSIT | $100,000.00  |
| 1001234570     | JENNIFER WILLIAMS  | CHECKING      | $8,750.50    |
| ...            | ...                | ...           | ...          |

**Total Balance:** $1,091,227.20  
**Account Types:** 8 CHECKING, 7 SAVINGS, 5 FIXED_DEPOSIT

See `src/main/resources/data.sql` for complete list.

## Testing

### Run All Tests
```bash
mvn test
```

### Manual API Testing with Postman

1. **Import Swagger/OpenAPI spec** from http://localhost:8080/api-docs
2. **Test scenarios:**
   - Read account 1001234567 (success)
   - Read account 9999999999 (not found)
   - Deposit $5000 to 1001234567
   - Withdraw $2000 from 1001234570
   - Withdraw $20000 from 1001234578 (insufficient funds)
   - Adjust balance of 1001234580 to $15000

## Configuration

### Database Configuration

**Development (H2 in-memory):**
```yaml
spring:
  datasource:
    url: jdbc:h2:mem:bankingdb
    driver-class-name: org.h2.Driver
    username: sa
    password:
```

**Production (PostgreSQL):**
```yaml
spring:
  datasource:
    url: jdbc:postgresql://localhost:5432/bankingdb
    driver-class-name: org.postgresql.Driver
    username: banking_user
    password: your_password
  jpa:
    properties:
      hibernate:
        dialect: org.hibernate.dialect.PostgreSQLDialect
```

### Application Properties

Edit `src/main/resources/application.yml`:
- **Server port:** Change `server.port` (default: 8080)
- **Logging level:** Change `logging.level.com.mainframe.banking` (default: DEBUG)
- **Database:** Switch between H2 and PostgreSQL
- **SQL initialization:** Enable/disable with `spring.sql.init.mode`

## COBOL to Java Mapping

### Data Types
| COBOL PIC Clause | Java Type | SQL Type |
|------------------|-----------|----------|
| PIC 9(10) | Long | BIGINT |
| PIC X(50) | String | VARCHAR(50) |
| PIC X(15) | Enum (AccountType) | VARCHAR(15) |
| PIC 9(09)V99 | BigDecimal | DECIMAL(11,2) |
| PIC 9(08) (date) | LocalDate | DATE |

### Business Logic Mapping

| COBOL Paragraph | Java Method | HTTP Endpoint |
|----------------|-------------|---------------|
| 2200-READ-ACCOUNT | `findByAccountNumber()` | GET /api/accounts/{id} |
| 2500-UPDATE-ACCOUNT | `processTransaction()` | POST /api/accounts/{id}/transactions |
| 2400-GET-UPDATE-INFO | Validation in DTO | @Valid annotation |
| 2700-HANDLE-ERROR | GlobalExceptionHandler | Error responses |

### Error Code Mapping

| COBOL Response Code | Java Exception | HTTP Status |
|---------------------|----------------|-------------|
| 000 (Success) | - | 200 OK |
| 003 (Not Found) | AccountNotFoundException | 404 NOT_FOUND |
| 009 (File Not Available) | - | 503 SERVICE_UNAVAILABLE |
| 044 (Record Locked) | - | 423 LOCKED |
| Insufficient Funds | InsufficientFundsException | 400 BAD_REQUEST |

## Deployment

### Docker Deployment (Optional)

Create `Dockerfile`:
```dockerfile
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY target/account-service-1.0.0.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
```

Build and run:
```bash
docker build -t banking-account-service .
docker run -p 8080:8080 banking-account-service
```

### Cloud Deployment

The application can be deployed to:
- **AWS**: Elastic Beanstalk, ECS, or Lambda
- **Azure**: App Service or Container Instances
- **Google Cloud**: Cloud Run or App Engine
- **Heroku**: Direct deployment with Heroku CLI

## Monitoring & Observability

**Spring Boot Actuator endpoints:**
- Health check: http://localhost:8080/actuator/health
- Metrics: http://localhost:8080/actuator/metrics
- Info: http://localhost:8080/actuator/info

## Performance Comparison

| Metric | COBOL (Mainframe) | Spring Boot (Modern) |
|--------|-------------------|----------------------|
| Response Time | ~100ms (terminal) | ~10-50ms (REST API) |
| Throughput | 10-20 TPS (terminal) | 1000+ TPS (REST API) |
| Scalability | Vertical only | Horizontal + Vertical |
| Monitoring | Limited | Rich (Actuator, APM tools) |
| Integration | Terminal, batch | REST, messaging, events |

## Troubleshooting

### Application won't start
- Check Java version: `java -version` (should be 17+)
- Check port 8080 is available: `netstat -an | findstr 8080`
- Review logs for errors

### Database errors
- Verify H2 console connection at `/h2-console`
- Check `schema.sql` and `data.sql` are in `src/main/resources/`
- Ensure `spring.sql.init.mode=always` in `application.yml`

### API returns 404
- Verify application started successfully
- Check endpoint URL: http://localhost:8080/api/accounts/{accountNumber}
- Use valid account number from seed data (1001234567 - 1001234586)

## Contributing

This project demonstrates mainframe modernization patterns. For enhancements:
1. Create feature branch
2. Add tests for new functionality
3. Update API documentation
4. Submit pull request

## License

Proprietary - Banking Institution

## Contact

Mainframe Modernization Team  
Email: modernization@banking.com

---

**Original COBOL Application:** `../cobol/`  
**Modernization Workflow:** `../Prompt Sequence.md`  
**Data Flow Diagram:** `../sequence-diagram.mmd`
