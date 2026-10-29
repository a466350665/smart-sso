# Frequently Asked Questions

**English** | [中文](faq.md)

## Deployment and runtime

### Why does the project use H2 by default? Could it reach production accidentally?

The `dev` profile uses an in-memory H2 database so the project can run with no external dependencies for demos,
integration work and automated tests.

For production, explicitly set `--spring.profiles.active=prod`. The production profile uses MySQL and does not run the
schema script automatically. If `smart.sso.server-url` is empty in an app that is not the SSO server, startup also
fails. These checks prevent a misconfigured application from running silently.

### Is data lost after a restart?

That is expected for the dev in-memory database. To persist data, use a file database and disable automatic initialization:

```yaml
spring:
  datasource:
    url: jdbc:h2:file:./data/smart-sso;MODE=MySQL
  sql:
    init:
      mode: never
```

### How do I switch to MySQL?

See the [Chinese README quick start](../README.zh-CN.md#方式二mysql生产) or the [configuration reference](configuration.en.md#5-database-script).
Import `smart-sso-server/src/main/resources/init.sql`, then start with `--spring.profiles.active=prod`.

### Why is the table prefix `sso_`?

`mybatis-plus.global-config.db-config.table-prefix` controls it centrally; entities do not hard-code table names. Change
the property and migrate the table names if a different prefix is required.

### How do I change the port or add a context path?

```bash
java -jar smart-sso-server-2.1.0.jar --server.port=18080 --server.servlet.context-path=/sso
```

Same-origin redirects and internal server calls automatically include the context path.

## Authentication and permissions

### Do I need to configure `smart.sso.server-url`?

- Standalone client: **yes**.
- An app that also contains the server, such as `smart-sso-server`: leave it empty for same-origin mode.
- Empty in an app that is not the server: startup fails with a missing-configuration error.

### Why are permissions not taking effect?

Permissions are declared by stable codes in `@RequiresPermission("...")`. The request must be inside
`smart.sso.url-patterns` and the user must be logged in. A logged-in user without the code receives HTTP 403 and
`000020`. Methods without the annotation are allowed after login. Permission sets are cached when a token is issued;
log in again or refresh the token after changing grants.

### How do I protect a back-end endpoint?

Add a stable permission code to the Controller method, for example `@RequiresPermission("organization:read")`, grant the
function permission under **Role Authorization**, and log in again. Unauthorized requests return HTTP 403 and
`{"code":"000020"}`.

### What do `000010`, `000015`, and `000020` mean?

| Code | Meaning | Action |
| --- | --- | --- |
| `000010` | Not logged in or the session expired | Clear local state and redirect to the authorization server. |
| `000015` | Access token expired but can be refreshed | Call the refresh endpoint and replay the request. |
| `000020` | Not authorized | Show an access-denied message. |

### How are user passwords stored?

The historical implementation uses `md5(plainText + fixedSalt "1qazx")`. The demo account is `admin` / `123456`.

> This is a legacy format with limited security. Replace `PasswordHelper` and migrate stored passwords when stronger
> password hashing is required.

### Why does single logout remove sessions from every application?

When a client exchanges a code, it registers its local logout URI with the server. During logout or forced logout, the
server revokes all tokens for the TGT and calls each registered URI so every client clears its local session. See the
[architecture guide](architecture.en.md#4-single-logout).

## User interface

### Why does `/admin/organization` return 404?

The admin console is a static SPA. Page fragments use the `.html` suffix and are loaded through a hash route, for example
`http://localhost:8080/admin/admin.html#/admin/organization.html`. Visiting `/` also forwards to the admin entry point.

### Why are some menu items missing after login?

The menu is derived from the current user's permissions returned by `/admin/admin/menu`. Add the required grants under
**Role Authorization** for the user's role.

## Development

### How can I run the verification suite?

```bash
verify/verify.sh                       # server protocol/security/API/CRUD checks
cd verify/e2e && BASE=http://127.0.0.1:8080 node front.mjs   # real-browser checks
```

See the [development guide](development.md#4-verification-suite).

### Can I build offline?

Yes, once the local Maven repository contains all dependencies:
`mvn -o -DskipTests clean package`. The first build may require a normal online Maven run.
