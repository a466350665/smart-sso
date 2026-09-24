# 开发与验证

## 一、环境准备

| 组件 | 版本 | 说明 |
| --- | --- | --- |
| JDK | 17+ | 编译与运行 |
| Maven | 3.8+ | 构建 |
| Node.js | 18+ | 仅前端验证套件需要 |
| Google Chrome | 任意较新版本 | 仅前端验证套件需要（复用本机浏览器，不额外下载） |

数据库与 Redis 都不是必需的：默认 `dev` profile 使用内置 H2 内存库。

## 二、构建与运行

```bash
# 构建（离线模式；本地仓库已具备依赖时无需联网）
mvn -o -DskipTests clean package

# 启动服务端（默认 dev / H2，端口 8080）
java -jar smart-sso-server/target/smart-sso-server-2.0.1.jar

# 启动演示客户端（端口 8082）
java -jar smart-sso-demo/target/smart-sso-demo-2.0.1.jar
```

常用参数：

```bash
java -jar smart-sso-server-2.0.1.jar --server.port=18080                 # 换端口
java -jar smart-sso-server-2.0.1.jar --spring.profiles.active=prod       # 使用 MySQL
java -jar smart-sso-server-2.0.1.jar --logging.level.openjoe.smart.sso=debug
```

> 若 `mvn -o` 提示缺少插件/依赖，去掉 `-o` 联网构建一次即可；沙箱或受限环境下需确保 Maven 可写本地仓库（`~/.m2/repository`）。

## 三、目录约定

```
smart-sso
├── smart-sso-server/          服务端（同时是自身的客户端）
│   └── src/main/
│       ├── java/openjoe/smart/sso/server/
│       │   ├── controller/           接口（admin 包为管理台接口）
│       │   ├── service/              业务逻辑
│       │   ├── entity/ mapper/       持久层（MyBatis-Plus）
│       │   └── dto/                  传输对象
│       └── resources/
│           ├── application*.yaml     公共配置 + dev/prod profile
│           ├── db/smart-sso.sql      跨库建表与演示数据脚本
│           └── static/               管理台静态 SPA（admin/ 为页面片段）
├── smart-sso-demo/            客户端接入示例
├── smart-sso-starter/         可独立引入的自动装配模块
└── verify/                    功能验证套件
```

管理台是静态 SPA：页面片段位于 `static/admin/*.html`，由 `static/index.html` 通过 hash 路由（`#/organization` → `/admin/organization.html`）加载。

## 四、验证套件

`verify/` 提供真实运行的验证（非 mock）：真实构建 → 真实启动 → 真实 HTTP / 真实浏览器。

```bash
# 服务端：构建 + 启动 dev/H2 + 协议/安全/接口/业务 CRUD 断言（退出码即结论）
verify/verify.sh

# 复用已启动实例（不构建不启停）
verify/verify.sh --external --base http://127.0.0.1:18080

# 前端：真实浏览器端到端（需要本机 Chrome）
cd verify/e2e && npm install && BASE=http://127.0.0.1:18080 node front.mjs
```

覆盖范围：

| 层 | 内容 |
| --- | --- |
| L0 | 构建、dev profile、H2 内存库、启动 |
| L1 | SSO 协议主链路：未登录跳转 → 登录页 → 换取令牌 → 下发 Cookie → 登出 → H5 入口 |
| L4 | 安全与权限：未登录分支（`302` / `000010`）、权限拦截（`000020`）、权限变更生效时机 |
| L2 | 只读接口全量（含校验类接口） |
| L3 | 业务 CRUD：应用/机构/角色（含授权与分配）/用户/权限，含级联删除；数据自建自清 |
| L5 | 前端：登录流程、菜单渲染、机构/用户/角色/权限页、权限树回显、弹窗行为 |

当前基线：**服务端 64 项断言 + 前端 31 项断言全部通过**。新增功能时建议在 `verify/verify.sh`（接口层）或 `verify/e2e/front.mjs`（界面层）补一条断言。

## 五、边界场景的手工验证

令牌过期与续签的两条分支需要短时效才能触发，可用 1 秒时长启动一个实例：

```bash
java -jar smart-sso-server-2.0.1.jar --server.port=18091 --smart.sso.server.access-token-timeout=1
# 登录后等 2 秒：
#   AJAX 请求（带 X-Requested-With）→ {"code":"000015"}
#   整页请求（不带该头）          → 200，客户端自动续签
```

## 六、文档中的图表

`README.md`、`README.en.md` 与 `docs/` 中的时序图使用 **Mermaid** 编写（GitHub / Gitee 原生渲染），
好处是图与代码一起维护，不会出现“图还是旧流程”的情况。修改流程时请同步修改对应的 Mermaid 代码块。

若需要在不支持 Mermaid 的平台（如某些博客/图床）使用图片，可本地导出 SVG/PNG：

```bash
cd verify/e2e
npm i -D @mermaid-js/mermaid-cli
# macOS 下通常需要指定本机 Chrome 并关闭沙箱
printf '{ "args": ["--no-sandbox", "--disable-setuid-sandbox"] }' > /tmp/pptr.json
PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
  npx mmdc -p /tmp/pptr.json -i ../../docs/architecture.md -o /tmp/mmd/
```

## 七、提交前自检

1. `mvn -o -DskipTests clean package` 通过；
2. `verify/verify.sh` 全部通过（退出码 0）；
3. 涉及界面改动时跑一遍 `verify/e2e` 前端套件；
4. 涉及数据库结构时同步更新 `db/smart-sso.sql`，并确认脚本对 MySQL 与 H2 均可用。
