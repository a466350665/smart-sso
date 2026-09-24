# 配置参考

## 一、Profile 总览

| Profile | 数据库 | 建表/演示数据 | 典型用途 |
| --- | --- | --- | --- |
| `dev`（默认） | H2 内存库 | 启动时自动执行 `db/smart-sso.sql` | 演示、联调、自动化测试 |
| `prod` | MySQL | **不执行**（Spring Boot 默认只初始化嵌入式库） | 生产 |

切换方式（三选一）：

```bash
java -jar smart-sso-server-2.0.1.jar --spring.profiles.active=prod
SPRING_PROFILES_ACTIVE=prod java -jar smart-sso-server-2.0.1.jar
# 或修改 application.yaml 中的 spring.profiles.active
```

## 二、客户端配置（`smart.sso.*`）

`smart-sso-starter-client` 的配置项（`ClientProperties`）：

| 配置项 | 类型 | 默认值 | 说明 |
| --- | --- | --- | --- |
| `server-url` | String | 空 | 认证中心地址。**留空 = 同源**，仅当本应用同时包含服务端时合法；独立客户端必须配置 |
| `internal-server-url` | String | 自动推导 | 同源模式下服务端之间调用使用的本机地址，默认 `http(s)://127.0.0.1:{实际端口}{context-path}` |
| `client-id` | String | — | 应用 ID，在服务端「应用管理」登记后获得 |
| `client-secret` | String | — | 应用密钥 |
| `url-patterns` | String[] | `/*` | 需要拦截的路径 |
| `exclude-urls` | String[] | 空 | 免登录路径。**以 `/*` 结尾表示前缀匹配**，否则精确匹配 |
| `order` | int | `10` | 过滤器顺序 |
| `name` | String | `clientContainer` | 过滤器注册名 |
| `logout-path` | String | `/logout` | 本应用的注销地址；服务端单点退出时会回调该路径 |
| `token-name-prefix` | String | `smart-sso-token-` | 令牌名前缀，实际名为 `前缀 + clientId`（如 `smart-sso-token-1000`） |

> 客户端还提供 `ClientContextHolder.getUserId() / getUser() / getPermission()` 获取当前登录信息。

## 三、服务端配置（`smart.sso.server.*`）

`smart-sso-starter-server` 的配置项（`ServerProperties`）：

| 配置项 | 类型 | 默认值 | 说明 |
| --- | --- | --- | --- |
| `timeout` | int | `7200` | 全局会话 TGT 与 refreshToken 超时（秒） |
| `access-token-timeout` | int | `1800` | accessToken 超时（秒） |
| `code-timeout` | int | `600` | 授权码超时（秒） |
| `cookie-name` | String | `TGC` | 服务端会话 Cookie 名 |
| `thread-pool-size` | int | `2` | 凭证过期清理线程池大小 |

## 四、持久层与表前缀

```yaml
mybatis-plus:
  global-config:
    db-config:
      table-prefix: sso_     # 表名 = sso_ + 实体名下划线形式
```

数据库共 7 张表：

| 表 | 说明 |
| --- | --- |
| `sso_app` | 应用（客户端）注册信息，含 clientId / clientSecret |
| `sso_organization` | 机构（树形，`parent_id` 自关联） |
| `sso_permission` | 权限（菜单/按钮），按 `app_id` 隔离，`url` 为权限标识 |
| `sso_role` | 角色 |
| `sso_role_permission` | 角色-权限关联 |
| `sso_user` | 用户（`organization_id` 关联机构） |
| `sso_user_role` | 用户-角色关联 |

## 五、数据库脚本

脚本位置：`smart-sso-server/src/main/resources/db/smart-sso.sql`。

- **跨库**：可直接用于 MySQL 与 H2（MySQL 兼容模式）；不含 `ENGINE` / `USING BTREE` 等专有语法；
- 自带演示数据：1 个认证中心自身应用（clientId `1000`）、1 个演示客户端（`1002`）、3 个机构、42 条权限、1 个「系统管理员」角色及其授权、1 个 `admin` 用户；
- 初始账号：**`admin` / `123456`**（口令算法见 [FAQ](faq.md#用户密码是怎么存的)）。

```bash
# MySQL 导入（注意指定字符集，避免中文乱码）
mysql -uroot -p -e "CREATE DATABASE IF NOT EXISTS \`smart-sso\` DEFAULT CHARSET utf8mb4"
mysql -uroot -p --default-character-set=utf8mb4 smart-sso < smart-sso-server/src/main/resources/db/smart-sso.sql
```

## 六、dev profile（H2）细节

```yaml
spring:
  datasource:
    url: jdbc:h2:mem:smart_sso;MODE=MySQL;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE
    driver-class-name: org.h2.Driver
    username: sa
    password: ''
  sql:
    init:
      mode: always
      schema-locations: classpath:db/smart-sso.sql
```

- 内存库，**重启即初始化**；如需保留数据可改为文件库 `jdbc:h2:file:./data/smart_sso;MODE=MySQL` 并关闭自动初始化（`spring.sql.init.mode=never`），否则每次启动都会重建；

## 七、配置覆盖优先级

Spring Boot 标准顺序，后者覆盖前者：`application.yaml` → `application-{profile}.yaml` → 环境变量 → 命令行参数。

```bash
java -jar smart-sso-server-2.0.1.jar \
  --server.port=8081 \
  --smart.sso.server.access-token-timeout=60 \
  --smart.sso.exclude-urls=/static/*,/sso/*,/auth/*,/login.html,/favicon.ico
```

## 八、服务端内置路径

| 路径 | 说明 |
| --- | --- |
| `/sso/login` | 登录入口（未登录时由客户端跳转至此） |
| `/sso/logout` | 退出入口，需带 `redirectUri` |
| `/sso/oauth2/access-token` | 授权码换取令牌 |
| `/sso/oauth2/refresh-token` | 刷新令牌 |
| `/sso/permission` | 拉取当前用户在本应用的权限 |
| `/auth/login_url`、`/auth/logout_url`、`/auth/access-token`、`/auth/refresh-token` | 供前端直接调用的认证入口 |
| `/` | 管理台（静态 SPA） |
