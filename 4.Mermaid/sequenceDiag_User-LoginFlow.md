## 🔐 User Login Flow – Architecture & Process

## This diagram illustrates how a user login request flows through the system components, ## ## including authentication and response with a JWT token.

```mermaid
sequenceDiagram
    participant User as 🧑 User (Browser)
    participant UI as 💻 Frontend UI (React/Blazor)
    participant API as 🛠️ Backend API (FastAPI/.NET)
    participant Auth as 🔒 Auth Service
    participant DB as 🗄️ User Database

    User->>UI: Submit login form (username & password)
    UI->>API: POST /login
    API->>Auth: Validate credentials
    Auth->>DB: Fetch user by username
    DB-->>Auth: Return user record + hashed password
    Auth-->>API: Valid/Invalid login + JWT token
    API-->>UI: Return JWT token or error
    UI-->>User: Store token (localStorage/Cookies)
  ```