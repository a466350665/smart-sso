# Development and Verification

**English** | [中文](development.md)

## 1. Prerequisites

| Component | Version | Notes |
| --- | --- | --- |
| JDK | 17+ | Build and runtime |
| Maven | 3.8+ | Build tool |
| Node.js | 18+ | Required only for browser verification |
| Google Chrome | Recent version | Required only for browser verification; the suite reuses the local browser and does not download one |

Neither a database nor Redis is required. The default `dev` profile uses an embedded H2 in-memory database.

## 2. Build and run

```bash
# Offline build once the local Maven repository contains all dependencies
mvn -o -DskipTests clean package

# Start the server (dev/H2, port 8080 by default)
java -jar smart-sso-server/target/smart-sso-server-2.1.0.jar

# Start the demo client (port 8082)
java -jar smart-sso-demo/target/smart-sso-demo-2.1.0.jar
```

Useful overrides:

```bash
java -jar smart-sso-server-2.1.0.jar --server.port=18080
java -jar smart-sso-server-2.1.0.jar --spring.profiles.active=prod
java -jar smart-sso-server-2.1.0.jar --logging.level.openjoe.smart.sso=debug
```

If `mvn -o` reports a missing plugin or dependency, run one online build without `-o`. In a sandbox or restricted
environment, Maven must be able to write to the local repository (`~/.m2/repository`).

## 3. Repository layout

```text
smart-sso
├── smart-sso-server/          Authorization server (also a client of itself)
│   └── src/main/
│       ├── java/openjoe/smart/sso/server/
│       │   ├── controller/           Controllers (`admin` contains console APIs)
│       │   ├── service/              Business services
│       │   ├── entity/ mapper/       Persistence layer (MyBatis-Plus)
│       │   └── dto/                  Data-transfer objects
│       └── resources/
│           ├── application*.yaml     Shared config plus dev/prod profiles
│           ├── init.sql              Cross-database schema and demo data
│           └── static/               Static admin SPA and page fragments
├── smart-sso-demo/            Example client
├── smart-sso-starter/         Independently consumable auto-configuration modules
└── verify/                    Functional verification suite
```

The admin console is a static SPA. Its entry point is `static/admin/admin.html`; page fragments live under
`static/admin/*.html` and are loaded through hash routes such as `#/admin/organization.html`.
The server forwards `/` to the admin entry point.

## 4. Verification suite

`verify/` performs real build, startup, HTTP, and browser checks; it does not use mocks for the application workflow.

```bash
# Server-side build, startup, protocol, security, API, and CRUD assertions
verify/verify.sh

# Verify an already running instance without building or starting it
verify/verify.sh --external --base http://127.0.0.1:18080

# Browser end-to-end checks (requires local Chrome)
cd verify/e2e && npm install && BASE=http://127.0.0.1:18080 node front.mjs

# Regenerate the admin screenshots used by the README
BASE=http://127.0.0.1:18080 node screenshots.mjs
```

| Layer | Coverage |
| --- | --- |
| L0 | Build, dev profile, H2 in-memory database, startup |
| L1 | SSO main flow: redirect, login page, code exchange, token cookie, logout, and front-end entry points |
| L4 | Security and permissions: unauthenticated `302`/`000010`, authorization failure `000020`, and permission refresh timing |
| L2 | All read-only and validation endpoints |
| L3 | Application, organization, role (including grants and assignment), user, and permission CRUD with cascade deletion |
| L5 | Browser login, menu rendering, admin pages, permission-tree state, and dialog behavior |

Current baseline: **68 server assertions + 37 browser assertions pass**. Add new API assertions to `verify/verify.sh`
and UI assertions to `verify/e2e/front.mjs`. When changing pages, regenerate `docs/images/` with `screenshots.mjs`.

## 5. Manual boundary checks

Token expiry branches require a short lifetime to trigger. Start a separate instance with a one-second access-token timeout:

```bash
java -jar smart-sso-server-2.1.0.jar \
  --server.port=18091 \
  --smart.sso.server.access-token-timeout=1
# After login, wait two seconds:
#   AJAX request with X-Requested-With -> {"code":"000015"}
#   Full-page request without the header -> HTTP 200; the client refreshes automatically
```

## 6. Diagrams in the documentation

The sequence diagrams in `README.md`, `README.zh-CN.md`, and `docs/` use **Mermaid**, which GitHub and Gitee render
natively. Keeping diagrams next to the documentation prevents them from drifting from the implementation. Update the
Mermaid block whenever a flow changes.

To export SVG or PNG for platforms without Mermaid support:

```bash
cd verify/e2e
npm i -D @mermaid-js/mermaid-cli
# macOS: point Puppeteer at the local Chrome and disable its sandbox
printf '{ "args": ["--no-sandbox", "--disable-setuid-sandbox"] }' > /tmp/pptr.json
PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
  npx mmdc -p /tmp/pptr.json -i ../../docs/architecture.en.md -o /tmp/mmd/
```

## 7. Pre-submit checklist

1. `mvn -o -DskipTests clean package` succeeds.
2. `verify/verify.sh` exits with code 0.
3. Run the browser suite when changing the UI.
4. When changing the database schema, update `init.sql` and confirm compatibility with both MySQL and H2.
