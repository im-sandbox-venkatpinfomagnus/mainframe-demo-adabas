---
agent: agent
model: claude-sonnet-4.5
tools: ['edit', 'runNotebooks', 'search', 'new', 'runCommands', 'runTasks', 'usages', 'vscodeAPI', 'problems', 'changes', 'testFailure', 'openSimpleBrowser', 'fetch', 'githubRepo', 'extensions', 'todos', 'runSubagent']
description: 'Modernize legacy COBOL mainframe applications to Spring Boot microservices with comprehensive tooling guide for Java 17+, Spring Boot 3.x, databases (H2/PostgreSQL/Oracle), Docker, CI/CD, and AI-assisted development using GitHub Copilot, Claude, or ChatGPT.'
---
# COBOL to Spring Boot Modernization - Reusable Prompt Template

## Overview
This template provides a structured approach to modernize legacy COBOL mainframe applications to modern Spring Boot microservices. Customize the placeholders marked with `[PLACEHOLDER]` based on your specific application.

---

## Prompt 1: API Design Specification

**Context:** I have a legacy COBOL application `[APPLICATION-NAME]` with `[NUMBER]` programs that perform `[BUSINESS-DOMAIN]` operations using `[DATABASE-TYPE]` database.

**COBOL Programs:**
- `[PROGRAM-1-NAME].cbl` - `[PROGRAM-1-DESCRIPTION]`
- `[PROGRAM-2-NAME].cbl` - `[PROGRAM-2-DESCRIPTION]`
- `[PROGRAM-N-NAME].cbl` - `[PROGRAM-N-DESCRIPTION]`

**Database Records:**
- `[ENTITY-NAME]`: `[FIELD-1]` ([TYPE]), `[FIELD-2]` ([TYPE]), `[FIELD-N]` ([TYPE])

**Business Operations:**
1. `[OPERATION-1]` - `[OPERATION-1-DESCRIPTION]`
2. `[OPERATION-2]` - `[OPERATION-2-DESCRIPTION]`
3. `[OPERATION-N]` - `[OPERATION-N-DESCRIPTION]`

**Request:** Design RESTful API endpoints for Spring Boot that replicate this functionality:

### Required Endpoints:

#### 1. GET /api/[resource]/{id}
- **Purpose:** `[DESCRIPTION]`
- **Response:** JSON with fields `[field1, field2, fieldN]`
- **Status Codes:** 200 (found), 404 (not found), 500 (error)

#### 2. POST /api/[resource]/{id}/[action]
- **Purpose:** `[DESCRIPTION]`
- **Request Body:** `{ "[field1]": [type], "[field2]": [type] }`
- **Validations:** `[validation-rules]`
- **Response:** Updated entity with timestamp
- **Status Codes:** 200 (success), 400 (validation error), 404 (not found)

#### 3. [ADDITIONAL-ENDPOINTS]

**Error Response Format:**
```json
{
  "timestamp": "ISO-8601 datetime",
  "status": "HTTP status code",
  "error": "Error type",
  "message": "User-friendly message",
  "path": "Request path"
}
```

---

## Prompt 2: Generate Complete Spring Boot Application

Generate a complete Spring Boot 3.x application that replicates the functionality of the COBOL programs:

### Project Setup:
- **Java Version:** 17 or 21
- **Build Tool:** Maven or Gradle
- **Spring Boot Version:** 3.2.x or higher
- **Dependencies:** 
  - Spring Web
  - Spring Data JPA
  - `[DATABASE-DEPENDENCY]` (H2, PostgreSQL, Oracle, etc.)
  - Validation
  - Lombok
  - Actuator
  - `[ADDITIONAL-DEPENDENCIES]`
- **Package Structure:** `com.[company].[domain].{controller, service, repository, model, dto, exception, config}`

### Database Layer:

**Entity: `[EntityName]`**
```java
@Entity
@Table(name = "[table_name]")
public class [EntityName] {
    @Id
    private [Type] [primaryKey];
    private [Type] [field1];
    private [Type] [field2];
    private BigDecimal [monetaryField];  // For currency fields
    private LocalDate [dateField];       // For date fields
    
    @CreatedDate
    private LocalDateTime createdDate;
    
    @LastModifiedDate
    private LocalDateTime modifiedDate;
}
```

**Repository: `[EntityName]Repository`**
- Extend JpaRepository
- Custom query methods: `[findByField1, findByField2And Field3, etc.]`
- Use `@Query` for complex queries if needed

**Database Initialization:**
- `schema.sql` - Create table DDL matching COBOL record structure
- `data.sql` - Load `[NUMBER]` test records from `[SOURCE-FILE]`
- Map COBOL PIC clauses to SQL types:
  - `PIC 9(n)` → BIGINT or INTEGER
  - `PIC 9(n)V99` → DECIMAL(n+2, 2)
  - `PIC X(n)` → VARCHAR(n)
  - `PIC 9(08)` (dates) → DATE

### Service Layer:

**Service: `[EntityName]Service`**

Methods to implement:
```java
public [Entity]DTO findBy[Field]([Type] [field]) 
    throws [Entity]NotFoundException;

public [Entity]DTO [operation1]([Type] [id], [DTO] request) 
    throws ValidationException, InsufficientFundsException;

public [Entity]DTO [operation2]([Type] [id], [DTO] request);

// Add methods for each COBOL program operation
```

**Business Rules (from COBOL logic):**
- `[RULE-1]`: `[DESCRIPTION]`
- `[RULE-2]`: `[DESCRIPTION]`
- `[RULE-N]`: `[DESCRIPTION]`

**Validations:**
- `[VALIDATION-1]`
- `[VALIDATION-2]`
- `[VALIDATION-N]`

**Transaction Management:**
- Use `@Transactional` for all update operations
- Update `[timestamp-field]` automatically on modifications
- Rollback on validation failures

### API Layer:

**Controller: `[EntityName]Controller`**
- Base path: `/api/[resource-path]`
- Implement endpoints from Prompt 1
- Use DTOs for all request/response bodies
- Validation annotations: `@Valid`, `@NotNull`, `@Min`, `@Max`, `@Pattern`

**DTOs:**
```java
public record [Entity]ResponseDTO(
    [Type] [field1],
    [Type] [field2],
    String formattedField  // For display formatting
) {}

public record [Operation]RequestDTO(
    @NotNull @Min([min]) BigDecimal amount,
    @Pattern(regexp = "[pattern]") String field
) {}
```

**Exception Handling:**
- `@RestControllerAdvice` global exception handler
- Custom exceptions: `[Entity]NotFoundException`, `[Custom]Exception`
- Map exceptions to appropriate HTTP status codes
- Include correlation IDs for tracing

### Configuration:

**application.yml:**
```yaml
spring:
  application:
    name: [application-name]
  datasource:
    url: jdbc:[database]:[connection-string]
    username: [username]
    password: [password]
  jpa:
    hibernate:
      ddl-auto: [none|validate|update]
    show-sql: [true|false]
    properties:
      hibernate:
        format_sql: true
  h2:
    console:
      enabled: [true|false]

server:
  port: [port-number]

logging:
  level:
    com.[company].[domain]: [DEBUG|INFO]
    org.hibernate.SQL: [DEBUG|INFO]
```

**Additional Configuration:**
- Enable Swagger/OpenAPI documentation
- CORS configuration if needed
- Security configuration (if applicable)
- Custom error response format

### Testing Requirements:

**Unit Tests:**
- Service layer tests with Mockito
- Repository tests with @DataJpaTest
- Controller tests with @WebMvcTest

**Integration Tests:**
- End-to-end API tests with @SpringBootTest
- Test data matching COBOL seed data
- Validate business rules and error scenarios

**Test Data (from COBOL seed-data.txt):**
```
[RECORD-1]: [field1]=[value1], [field2]=[value2]
[RECORD-2]: [field1]=[value1], [field2]=[value2]
[RECORD-N]: [field1]=[value1], [field2]=[value2]
```

### Documentation Requirements:

Generate:
1. README.md with setup instructions
2. API documentation (Swagger UI at /swagger-ui.html)
3. Data migration guide from COBOL flat files
4. Deployment guide
5. Performance comparison notes

### Code Quality Standards:

- Follow Spring Boot best practices
- Use constructor injection (not field injection)
- Implement proper logging with SLF4J
- Add JavaDoc for public methods
- Use Java records for immutable DTOs
- Implement equals/hashCode/toString for entities
- Add @NotNull/@Nullable annotations where appropriate

---

## Prompt 3: Migration Strategy & Testing

**Request:** Provide a detailed migration strategy from COBOL to Spring Boot:

### Phase 1: Parallel Run
- Deploy Spring Boot alongside existing COBOL system
- Route `[PERCENTAGE]`% of traffic to new API
- Compare responses for data consistency
- Monitoring and alerting setup

### Phase 2: Data Migration
- Strategy for migrating from `[OLD-DATABASE]` to `[NEW-DATABASE]`
- ETL scripts for data transformation
- Validation queries to ensure data integrity
- Rollback procedures

### Phase 3: Cutover
- Traffic routing strategy
- Smoke tests for critical paths
- Rollback plan if issues arise
- Communication plan for stakeholders

### Testing Strategy:
- **Functional Testing:** Test cases matching COBOL scenarios
- **Performance Testing:** Load test with `[EXPECTED-TPS]` transactions/second
- **Security Testing:** Authentication, authorization, input validation
- **Data Validation:** Compare outputs between COBOL and Spring Boot for `[TEST-SCENARIOS]`

---

## Usage Instructions

1. **Fill in all `[PLACEHOLDERS]`** with your specific application details
2. **Customize endpoints** based on your business requirements
3. **Adjust database** configuration for your target environment
4. **Add domain-specific** business rules and validations
5. **Run prompts sequentially** with an AI assistant (Copilot, ChatGPT, Claude)
6. **Review and test** generated code thoroughly
7. **Iterate** on implementation based on feedback

## Example: Banking Application

See `modernization-example-banking.md` for a complete example using this template for the COBOL banking application in this project.

---

## Related Files
- `.github/copilot-instructions.md` - COBOL coding conventions
- `README.md` - Original COBOL application documentation
- `Prompt Sequence.md` - Detailed modernization workflow
- `sequence-diagram.mmd` - Data flow visualization
