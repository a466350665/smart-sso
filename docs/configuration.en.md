# Configuration Reference

**English** | [中文](configuration.md)

## 1. Profiles

| Profile | Database | Schema/demo data | Typical use |
| --- | --- | --- | --- |
| `dev` (default) | H2 in-memory | Runs `init.sql` on startup | Demo, integration work and automated tests |
| `prod` | MySQL | **Not run** (Spring Boot initializes embedded databases only by default) | Production |

Choose a profile with any of these methods:

```bash
java -jar smart-sso-server-2.1.0.jar --spring.profiles.active=prod
SPRING_PROFILES_ACTIVE=prod java -jar smart-sso-server-2.1.0.jar
# Or set spring.profiles.active in application.yaml
```

## 2. Client properties (`smart.sso.*`)

Properties provided by `smart-sso-starter-client` (`ClientProperties`):

| Property | Type | Default | Description |
| --- | --- | --- | --- |
| `server-url` | String | empty | Authorization-server URL. **Empty means same-origin** and is valid only when the app also contains the server. Standalone clients must set it. |
| `internal-server-url` | String | derived automatically | Local address used for server-to-server calls in same-origin mode: `http(s)://127.0.0.1:{port}{context-path}`. |
| `client-id` | String | - | Application ID registered in Application Management. |
| `client-secret` | String | - | Application secret. |
| `url-patterns` | String[] | `/*` | Protected paths; supports exact paths and `/*` prefix paths. |
| `exclude-urls` | String[] | empty | Paths that bypass SSO completely. A trailing `/*` enables prefix matching. |
| `order` | int | `10` | Filter order. |
| `name` | String | `clientContainer` | Filter registration name. |
| `logout-path` | String | `/logout` | Local callback path used by server-side single logout. |
| `token-name-prefix` | String | `smart-sso-token-` | Token prefix; actual name is prefix plus `clientId`, for example `smart-sso-token-1000`. |

Use `ClientContextHolder.getUserId()`, `getUser()`, and `getPermission()` to access the current login information.

## 3. Server properties (`smart.sso.server.*`)

Properties provided by `smart-sso-starter-server` (`ServerProperties`):

| Property | Type | Default | Description |
| --- | --- | --- | --- |
| `timeout` | int | `7200` | TGT and refresh-token lifetime in seconds. |
| `access-token-timeout` | int | `1800` | Access-token lifetime in seconds. |
| `code-timeout` | int | `600` | Authorization-code lifetime in seconds. |
| `cookie-name` | String | `TGC` | Server session cookie name. |
| `thread-pool-size` | int | `2` | Credential-expiration cleanup pool size. |

## 4. Persistence and table prefix

```yaml
mybatis-plus:
  global-config:
    db-config:
      table-prefix: sso_     # table name = sso_ + underscored entity name
```

The schema contains seven tables:

| Table | Description |
| --- | --- |
| `sso_app` | Registered client applications, including client ID and secret. |
| `sso_organization` | Organization tree with `parent_id` self-reference. |
| `sso_permission` | Menu/function permissions isolated by `app_id`; stable `code`, menu address in `url`, `type` is `menu` or `function`. |
| `sso_role` | Roles. |
| `sso_role_permission` | Role-permission mapping. |
| `sso_user` | Users linked to organizations through `organization_id`. |
| `sso_user_role` | User-role mapping. |

## 5. Database script

The script is `smart-sso-server/src/main/resources/init.sql`.

- It works with MySQL and H2 in MySQL compatibility mode and avoids vendor-specific `ENGINE`/`USING BTREE` syntax.
- It includes the authorization-server application (`clientId=1000`), demo client (`1002`), three organizations, 42
  permissions, the System Administrator role and its grants, and the `admin` user.
- Initial credentials are **`admin` / `123456`**. See the [FAQ](faq.en.md#how-are-user-passwords-stored) for the hash format.

```bash
# Import into MySQL; specify the character set to avoid corrupted Chinese text.
mysql -uroot -p -e "CREATE DATABASE IF NOT EXISTS \`smart-sso\` DEFAULT CHARSET utf8mb4"
mysql -uroot -p --default-character-set=utf8mb4 smart-sso < smart-sso-server/src/main/resources/init.sql
```

## 6. Details of the dev H2 profile

```yaml
spring:
  datasource:
    url: jdbc:h2:mem:smart-sso;MODE=MySQL;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE
    username: sa
    password: ''
  sql:
    init:
      mode: always
      schema-locations: classpath:init.sql
```

The in-memory database is re-created on every restart. To keep data, use a file database such as
`jdbc:h2:file:./data/smart-sso;MODE=MySQL` and disable initialization with `spring.sql.init.mode=never`.

## 7. Configuration precedence

Spring Boot's standard order applies; later sources override earlier ones:
`application.yaml` -> `application-{profile}.yaml` -> environment variables -> command-line arguments.

```bash
java -jar smart-sso-server-2.1.0.jar \
  --server.port=8081 \
  --smart.sso.server.access-token-timeout=60 \
  --smart.sso.exclude-urls=/static/*,/sso/*,/auth/*,/login.html,/favicon.ico
```

## 8. Built-in server paths

| Path | Description |
| --- | --- |
| `/sso/login` | Login entry point. Clients redirect here when no session exists. |
| `/sso/logout` | Logout entry point; accepts `redirectUri`. |
| `/sso/oauth2/access-token` | Exchange an authorization code for tokens. |
| `/sso/oauth2/refresh-token` | Refresh a token. |
| `/sso/permission` | Fetch permissions for the current user and application. |
| `/auth/login_url`, `/auth/logout_url`, `/auth/access-token`, `/auth/refresh-token` | Authentication helpers for front ends. |
| `/` | Static admin SPA. |
