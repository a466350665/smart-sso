# 客户端接入指南

## 一、引入依赖

```xml
<dependency>
    <groupId>io.github.openjoe</groupId>
    <artifactId>smart-sso-starter-client</artifactId>
    <version>2.0.1</version>
</dependency>

<!-- 多实例（分布式）场景：共享令牌与权限 -->
<dependency>
    <groupId>io.github.openjoe</groupId>
    <artifactId>smart-sso-starter-client-redis</artifactId>
    <version>2.0.1</version>
</dependency>
```

不引入 Redis 模块时使用本地内存实现，仅适合单实例部署。

## 二、登记应用

在认证中心「应用管理」中新增应用，获得 `clientId` 与 `clientSecret`；同时在「权限管理」中为该应用登记菜单与按钮权限（`url` 即权限标识）。

## 三、最小配置

```yaml
smart:
  sso:
    # 认证中心地址。独立客户端必填；若本应用同时是服务端可留空（同源模式）
    server-url: http://localhost:8080
    client-id: 1000
    client-secret: rokY9BdKh5bHiX/zL26qOg==
    # 免登录路径（以 /* 结尾为前缀匹配）
    exclude-urls: /static/*,/sso/*,/auth/*,/login.html,/favicon.ico
```

启动后，客户端会自动装配过滤器链：

| 过滤器 | 职责 |
| --- | --- |
| `LogoutFilter` | 处理服务端单点退出的回调（`smart.sso.logout-path`，默认 `/logout`） |
| `LoginFilter` | 无令牌则跳转认证中心；处理回跳的 `code` 换取令牌；令牌过期时按请求类型续签或返回 `000015` |
| `PermissionFilter` | 按权限快照判断请求路径，无权限返回 `000020` |

## 四、获取当前登录信息

```java
Long userId = ClientContextHolder.getUserId();
TokenUser user = ClientContextHolder.getUser();
TokenPermission permission = ClientContextHolder.getPermission();   // 权限集合 + 菜单列表
```

## 五、前端约定的返回码

接口以 JSON 返回业务码，前端需要识别以下三个：

| code | 含义 | 前端应做的处理 |
| --- | --- | --- |
| `000010` | 未登录或会话超时 | 清理本地缓存并跳转认证中心 |
| `000015` | accessToken 过期、refreshToken 仍有效 | 调用刷新接口获取新令牌后**重放原请求**，刷新失败则跳转认证中心 |
| `000020` | 已登录但无访问权限 | 提示无权限 |

```js
// 请求需带上令牌与 X-Requested-With，客户端才能识别为前端请求
headers: {
  'smart-sso-token-1000': localStorage.getItem('accessToken1000'),
  'X-Requested-With': 'XMLHttpRequest'
}
```

> `X-Requested-With` 很关键：客户端据此区分“前端请求（回 JSON）”与“浏览器整页请求（自动续签）”。
> jQuery 同源请求会自动携带；跨域或用 `fetch` 时需自行设置。

服务端另外提供 `/auth/login_url`、`/auth/logout_url`、`/auth/access-token`、`/auth/refresh-token` 四个接口，供前端直接调用（同源模式下返回的登录/退出地址是相对路径）。

## 六、跨域场景

- 令牌可以走请求头（`smart-sso-token-{clientId}`）而非 Cookie；
- 前端请求需自行设置 `X-Requested-With`（跨域时浏览器不会自动带上）；
- 客户端的 `exclude-urls` 需要放行前端直接调用的接口（如 `/auth/*`）。

## 七、常见接入问题

| 现象 | 原因与处理 |
| --- | --- |
| 启动报错「未配置 smart.sso.server-url，且当前应用不是 SSO 服务端」 | 独立客户端必须配置 `server-url`；该 fail-fast 是为了避免把用户跳转到客户端自己的 `/sso/login` |
| 登录后接口仍返回 `000020` | 该请求路径已登记到权限表但未授权给当前用户；到「角色授权」中勾选并重新登录 |
| 登记了权限却不生效 | 请求路径与 `sso_permission.url` 需**完全相等**（区分 `/a/b` 与 `/a/b.html`）；另外权限快照在登录时生成，改权限后需重新登录或刷新令牌 |
| 未登记权限的接口也能访问 | 未登记的路径默认放行，这是设计如此；需要管控就必须登记 |
| 中文或表单提交异常 | 确认服务端与数据库连接字符集为 `utf8mb4` |
| 前端页面 404 | 管理台是 SPA，页面需带 `.html` 后缀或通过菜单的 hash 路由访问（见 [FAQ](faq.md)） |

## 八、参考实现

- `smart-sso-demo`：客户端接入示例（端口 8082，令牌走请求头）；
- `smart-sso-server` 自身：同源模式下“服务端即客户端”的用法，`server-url` 留空即可。
