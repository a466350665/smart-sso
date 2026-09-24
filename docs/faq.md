# 常见问题

## 部署与运行

### 为什么默认用 H2？会不会误上生产？

`dev` profile 使用内存 H2，目的是让项目**零外部依赖即可跑起来**（演示、联调、自动化测试）。

生产请显式指定 `--spring.profiles.active=prod`，此时使用 MySQL 且**不会自动执行建表脚本**（`spring.sql.init.mode=never`）。另外，如果 `smart.sso.server-url` 留空而本应用不是服务端，应用会**启动失败**——都是为了避免“配错却静默运行”。

### 数据重启就没了？

dev/内存库的正常行为。若要保留数据，把数据源改为文件库并关闭自动初始化：

```yaml
spring:
  datasource:
    url: jdbc:h2:file:./data/smart_sso;MODE=MySQL
  sql:
    init:
      mode: never
```

### 怎么切换到 MySQL？

见 [README 快速开始](../README.md#方式二mysql生产) 或 [configuration.md](configuration.md#五数据库脚本)。要点：先导入 `smart-sso-server/src/main/resources/db/smart-sso.sql`，再用 `--spring.profiles.active=prod` 启动。

### 表名前缀为什么是 `sso_`？

由 `mybatis-plus.global-config.db-config.table-prefix` 统一控制，实体上不写死表名。要改前缀只需改这一处配置 + 迁移表名。

### 端口被占用 / 想加 context-path？

```bash
java -jar smart-sso-server-2.0.1.jar --server.port=18080 --server.servlet.context-path=/sso
```

同源模式下页面跳转是相对路径、会自动带上 context-path；服务端之间的调用地址也会带上它。

## 认证与权限

### `smart.sso.server-url` 到底要不要配？

- 独立客户端：**必须配置**；
- 本应用同时是服务端（如 `smart-sso-server`）：**留空即可**，自动按同源处理（页面跳转走相对路径、服务端调用走 `http://127.0.0.1:{port}`）；
- 留空但本应用不是服务端：启动直接失败，提示你补配置。

### 权限配了为什么没用？

三个常见原因：

1. **路径不相等**：权限判断是 `request.getServletPath()` 与 `sso_permission.url` 的**精确匹配**。登记 `/organization.html` 不会拦住 `/admin/organization.html` 或 `/admin/organization/list`；
2. **未登记则放行**：没有登记进权限表的路径默认放行，这是设计行为；
3. **快照未刷新**：权限集合在登录/换取令牌时生成并缓存，改完权限需**重新登录**（或等令牌刷新）才生效。

### 怎么让后端接口也受权限控制？

把接口路径登记为权限（例如 `url=/admin/organization/list`），并在「角色授权」中按需勾选/取消，然后重新登录。这样做后，未被授权的用户访问该接口会得到 `{"code":"000020"}`。

### 前端拿到的 `000010` / `000015` / `000020` 是什么？

| code | 含义 | 处理 |
| --- | --- | --- |
| `000010` | 未登录/会话超时 | 清缓存并跳认证中心 |
| `000015` | accessToken 过期但可续签 | 调刷新接口后重放请求 |
| `000020` | 无访问权限 | 提示用户 |

### 用户密码是怎么存的？

`md5(明文 + 固定盐 "`1qazx")`。演示账号 `admin` 的初始口令为 `123456`。

> 这是历史实现，安全性有限；如需更高强度请替换 `PasswordHelper` 并升级存量口令。

### 单点退出为什么能踢掉所有应用？

客户端在换取令牌时会把自身的注销地址（`getLocalUrl() + smart.sso.logout-path`）登记到服务端。退出或强制下线时，服务端吊销该 TGT 下的令牌，并**逐个回调**这些地址通知客户端清理本地会话。详见 [architecture.md](architecture.md#四单点退出时序)。

## 界面

### 直接访问 `/admin/organization` 为什么 404？

管理台是静态 SPA，页面片段带 `.html` 后缀，且通过 hash 路由加载：应访问 `http://localhost:8080/#/organization.html`（或从菜单点击）。

### 启用了 H2 控制台却打不开？

H2 控制台仅在 dev 开启，且需要放行路径。默认配置已包含 `/h2-console` 与 `/h2-console/*`；如果你自定义了 `smart.sso.exclude-urls`，请把这两项加回去。

### 登录后菜单少了几项？

菜单由当前用户的权限决定（服务端 `/admin/admin/menu` 返回）。到「角色授权」为用户所属角色补权限即可。

## 开发

### 如何本地跑验证，确认改动没有破坏已有功能？

```bash
verify/verify.sh                       # 服务端：协议/安全/接口/CRUD
cd verify/e2e && BASE=http://127.0.0.1:8080 node front.mjs   # 前端：真实浏览器
```

详见 [development.md](development.md#四验证套件)。

### 能离线构建吗？

可以，前提是本地 Maven 仓库已具备依赖：`mvn -o -DskipTests clean package`。首次请联网构建一次。
