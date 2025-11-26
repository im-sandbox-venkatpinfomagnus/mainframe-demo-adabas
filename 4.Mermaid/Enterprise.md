## Project Scale Comparison
```mermaid
flowchart LR
    A[Small Project] --> B[Single Organization]
    B --> C[Few Repos]
    B --> D[Teams: Devs, Ops]
    A --> E[Large Company]
    E --> F[Enterprise]
    F --> G[Multiple Organizations]
    G --> H[Teams Mapped to Squads]
    G --> I[Central Security Policies]
```
**Small project:** single Organization with a few repos. Teams: devs, ops.

**Large company:** Enterprise with multiple Organizations (by department or legal entity). Each org has teams mapped to squads and central enterprise security policies applied.

## Enterprise Account Structure
```mermaid
flowchart LR
    subgraph Enterprise[Enterprise Account]
        direction TB
        Org1[Organization A]
        Org2[Organization B]
    end

    subgraph Org[Organization]
        direction TB
        Team1(Team: Backend)
        Team2(Team: Frontend)
        Repo1[Repository: api]
        Repo2[Repository: web]
        Owners((Org Owners))
    end

    Dev1(Developer: Alice)
    Dev2(Developer: Bob)

    Dev1 -->|member of| Team1
    Dev2 -->|member of| Team2

    Team1 -->|permission: write| Repo1
    Team2 -->|permission: write| Repo2
    Owners -->|admin| Repo1
    Owners -->|admin| Repo2

    Enterprise --> Org1
    Enterprise --> Org2
    Org1 --> Org
    Org2 --> Org

    style Enterprise fill:#f5f7ff,stroke:#3b82f6,color:#ff0000
    style Org fill:#fdf7ed,stroke:#f59e0b,color:#ff0000
    style Team1 fill:#eef2ff,stroke:#6366f1,color:#ff0000
    style Team2 fill:#eef2ff,stroke:#6366f1,color:#ff0000
    style Dev1 fill:#ecfccb,stroke:#65a30d,color:#ff0000
    style Dev2 fill:#ecfccb,stroke:#65a30d,color:#ff0000
```
