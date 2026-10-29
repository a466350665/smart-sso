# Client Integration Guide

**English** | [中文](client-integration.md)

## 1. Add the dependency

```xml
<dependency>
    <groupId>io.github.openjoe</groupId>
    <artifactId>smart-sso-starter-client</artifactId>
    <version>2.1.0</version>
</dependency>

<!-- Multi-instance deployment: share tokens and permissions through Redis -->
<dependency>
    <groupId>io.github.openjoe</groupId>
    <artifactId>smart-sso-starter-client-redis</artifactId>
    <version>2.1.0</version>
</dependency>
```

Without the Redis module, the client uses local in-memory storage and is suitable only for a single instance.

## 2. Register the application

Create an application in **Application Management** on the authorization server to obtain a `clientId` and
`clientSecret`. In **Permission Management**, register the menu and function permissions used by the application;
`code` is the stable permission identifier.

## 3. Minimal configuration

```yaml
smart:
  sso:
    # Required for a standalone client. Leave empty when this app also contains the SSO server.
    server-url: http://localhost:8080
    client-id: 1000
    client-secret: rokY9BdKh5bHiX/zL26qOg==
    # Paths that do not require login. A trailing /* enables prefix matching.
    exclude-urls: /static/*,/sso/*,/auth/*,/login.html,/favicon.ico
```

After startup, the client automatically registers the following filter and interceptor chain:

| Component | Responsibility |
| --- | --- |
| `SsoLogoutFilter` | Handles server-side single logout callbacks at `smart.sso.logout-path` (default `/logout`). |
| `SsoLoginFilter` | Redirects requests without a token to the authorization server, exchanges the callback `code` for a token, and refreshes expired tokens or returns `000015` according to the request type. |
| `SsoAuthorizationInterceptor` | Checks stable permission codes declared by `@RequiresPermission`; unauthorized requests return HTTP 403 and `000020`. |

## 4. Get the current user

```java
Long userId = ClientContextHolder.getUserId();
TokenUser user = ClientContextHolder.getUser();
TokenPermission permission = ClientContextHolder.getPermission();   // permissions and menu entries
```

## 5. Front-end response codes

APIs return a JSON business code. The front end should handle these three codes:

| Code | Meaning | Client action |
| --- | --- | --- |
| `000010` | Not logged in or the session has expired | Clear local state and redirect to the authorization server. |
| `000015` | The access token expired while the refresh token is still valid | Refresh the token and **replay the original request**; redirect to login if refresh fails. |
| `000020` | Logged in but not authorized | Show an access-denied message. |

```js
// Include the token and X-Requested-With so the client can identify an AJAX request.
headers: {
  'smart-sso-token-1000': localStorage.getItem('accessToken1000'),
  'X-Requested-With': 'XMLHttpRequest'
}
```

`X-Requested-With` is important: the client uses it to distinguish an AJAX request (return JSON) from a full-page
browser request (refresh automatically). Same-origin jQuery requests add it automatically; cross-origin requests and
`fetch` calls must set it explicitly.

The server also exposes `/auth/login_url`, `/auth/logout_url`, `/auth/access-token`, and `/auth/refresh-token` for
front-end clients. In same-origin mode, login and logout URLs are relative paths.

## 6. Cross-origin deployments

- Send the token in the `smart-sso-token-{clientId}` request header instead of using a cookie.
- Set `X-Requested-With` explicitly; browsers do not add it automatically to cross-origin requests.
- Add paths used by direct front-end calls, such as `/auth/*`, to `exclude-urls`.

## 7. Common integration issues

| Symptom | Cause and fix |
| --- | --- |
| Startup fails because `smart.sso.server-url` is missing and the app is not an SSO server | A standalone client must configure `server-url`. The fail-fast behavior prevents redirects to the client's own `/sso/login`. |
| APIs still return `000020` after login | The stable permission code declared by the Controller method is not assigned to the current user. Grant it under **Role Authorization**, then log in again. |
| Permission annotations have no effect | Check that the request matches `smart.sso.url-patterns` and is not excluded by `exclude-urls`. Permission snapshots are created at login/token refresh, so refresh the token after changing permissions. |
| An endpoint without a permission annotation is still accessible | This is intentional: endpoints inside the protected range are allowed after login unless they declare `@RequiresPermission`. Add the annotation for fine-grained protection. |
| Chinese text or form submissions are corrupted | Ensure the server and database connection use `utf8mb4`. |
| A front-end page returns 404 | The admin console is an SPA. Use the `.html` page fragment or the hash route from the menu; see the [FAQ](faq.en.md). |

## 8. Reference implementations

- `smart-sso-demo`: client example on port 8082, sending the token in a request header;
- `smart-sso-server`: the server acting as its own client in same-origin mode, with an empty `server-url`.
