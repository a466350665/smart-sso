# Smart-SSO 全量功能验证报告

- 验证对象：`smart-sso-server` 2.0.1（dev profile / H2 内存库，jar 直接启动）
- 验证方式：真实构建 + 真实启动 + 真实 HTTP / 真实浏览器
- 验证时间：本次会话
- 结论：**服务端 64 项通过、0 失败；前端 31 项通过；过程中发现的 3 个缺陷已全部修复并加入回归用例**

## 一、怎么跑

```bash
# 服务端 L0~L4（自动构建 + 启动 dev/H2 + 全部断言 + 关闭）
verify/verify.sh

# 复用已启动的实例（不构建不启停）
verify/verify.sh --external --base http://127.0.0.1:18090

# 前端 L5（需要本机 Chrome，已装 playwright-core）
cd verify/e2e && BASE=http://127.0.0.1:18090 node front.mjs
```

> 前置：`mvn -o -DskipTests clean package` 能通过（离线即可，本地仓库需已具备依赖）。

## 二、结果总览

| 层 | 覆盖内容 | 结果 |
|---|---|---|
| L0 构建与启动 | 9 模块构建、dev profile、H2 内存库、启动耗时 | 4/4 ✅ |
| L1 SSO 协议主链路 | 未登录跳转、登录页、code 换 token、Cookie、登出、H5 入口 | 10/10 ✅ |
| L4 安全与权限 | 未登录 302/000010、权限拦截 000020、权限变更需重登 | 7/7 ✅ |
| L2 只读接口 | 15 个 GET/校验接口全量 | 18/18 ✅ |
| L3 业务 CRUD | 应用/机构/角色/用户/权限 的新增·修改·启禁用·授权·级联删除 | 20/20 ✅ |
| L5 前端（浏览器） | 登录流程、菜单、5 个业务页面、权限树、弹窗收敛 | 28/28 ✅ |
| L6 边界（token 刷新） | AJAX vs 整页 两条分支 | 2/2 ✅ |
| 缺陷回归 | 第四节 3 个缺陷的定点用例 | 7/7 ✅ |

## 三、本次改造的回归验证（重点）

| 改造点 | 验证方式 | 结果 |
|---|---|---|
| 默认 H2、jar 开箱即用 | `java -jar` 直启，日志确认 `profile=dev` + `jdbc:h2:mem:smart_sso` | ✅ |
| 表前缀 `sso_` | 运行期 SQL 日志：`FROM sso_app/sso_user/sso_role_permission…` | ✅ |
| `office → organization` | 接口 `/admin/organization/*`、用户字段 `organizationId`、前端机构下拉/树 | ✅ |
| 同源（不配 server-url） | `/auth/login_url` 返回 `/sso/login?...`、`logoutUrl` 为 `/sso/logout?...`；callback 全程无需配置域名 | ✅ |
| 服务端自我调用（路线 A） | 回跳时用 code 换 token 成功并下发 cookie（该步即本机自调用） | ✅ |
| 去掉 `h5-enabled` | 1 秒 TTL 实测：AJAX → `000015`；整页 → 200 自动刷新无感 | ✅ |
| 弹窗收敛 | 浏览器实测：行内启用/禁用**无任何提示**，且状态列即时更新；删除仍弹确认框 | ✅ |
| 机构编辑父机构默认选中 | 浏览器实测 `#_parentId = 1`（TT公司） | ✅ |
| 角色授权权限树默认勾选 | 浏览器实测 36 节点中 35 个已勾选 | ✅ |
| 权限弹窗/Toast 收敛 | 对照组（未选行点禁用）仍提示“至少选择一行”，证明检测有效 | ✅ |

## 四、发现并修复的缺陷（均已有回归用例）

### 1. 空 `ids` 触发 SQL 语法错误 → 已修复

- **原现象**：`POST /admin/user/enable`（`ids=`）→ `{"code":"000001"}`；日志为
  `SELECT ... WHERE (id IN ())` / `DELETE FROM sso_user_role WHERE (user_id IN ())`（H2 与 MySQL 均不接受）。
- **修复**：在 `AppService` / `OrganizationService` / `RoleService` / `UserService` 的
  `enable`、`deleteByIds` 入口加空集合短路（`CollectionUtils.isEmpty(idList)` 直接返回）；
  `OrganizationController.delete` 改调新增的 `OrganizationService.deleteByIds`。
- **回归**：`verify.sh` 中 6 条“空 ids”用例（用户启禁用/删除、机构启禁用/删除、角色启禁用、应用删除）全部 `000000`。

### 2. “未完成换 token”的登录导致在线用户页持续报错 → 已修复

- **原现象**：发起 `/sso/login` 拿到 TGT 但不访问 callback（用户关浏览器、回跳失败），此后
  `GET /admin/login-user/list` 一直 `000001`，直到 TGT 超时（默认 7200s）。
- **根因**：
  `LocalTokenManager.getClientIdMapByTgt` 未判空 `tgtMap.get(tgt)` →
  `NullPointerException: ... "refreshTokenSet" is null`（Redis 实现本来就有判空，Local 漏了）。
- **修复**：
  - `LocalTokenManager`：`refreshTokenSet` 为空时按空集合处理（仍写入 tgt 键，保证调用方不为 null）；
  - `LoginUserService.convertList`：`userMap.get(...)` 为 null（用户被删）时跳过该条；`clientIdMap.get(tgt)` 为空、
    `appMap.get(clientId)` 为 null（应用被删）时按空处理。
- **回归**：`verify.sh` 先制造一次“只换 TGT 不走回跳”的登录，再断言 `/admin/login-user/list` 仍为 `000000`。

### 3. 权限树缺 `isMenu` 导致“添加子权限”按钮永不出现 → 已修复

- **原现象**：权限管理页任何节点都没有「添加」按钮，无法新增子权限。
- **根因**：`PermissionDTO extends TreeDTO` 未包含业务字段，`BeanUtils.copyProperties` 丢弃 `isMenu`，
  而 `permission.html:303` 依据 `treeNode.isMenu` 渲染「添加」按钮。
- **修复**：`PermissionDTO` 增加 `isMenu` 字段。
  **刻意不加 `icon` 与 `url`**：zTree 会把 `node.icon` 当图片路径、把 `node.url` 当超链接，
  加上后会分别产生 `/fa-*` 的 404 请求与点击误跳转（本次实测踩到了）；编辑表单需要的完整字段走
  `/admin/permission/get` 获取。
- **回归**：浏览器用例断言「权限树节点带 `isMenu`」+「菜单节点悬停出现添加按钮」+「不暴露 icon/url」，
  并断言全流程无 4xx/5xx。

## 五、其它观察（非缺陷，供参考）

1. `GET /admin/organization/all` 只返回**启用**的机构（`isEnable=true`），禁用后从该接口消失；列表页用的是 `/admin/organization/list`（不过滤）。接口命名易误解，建议注释或改名。
2. `GET /admin/organization/list` 的 `current/size` 是必填但未使用，漏传返回 `000002`；建议给默认值或去掉。
3. **权限拦截只在请求路径与 `sso_permission.url` 完全相等时生效**。种子数据登记的是 `xxx.html` 形式的 UI 路径，而 SPA 实际请求的是 `/admin/xxx.html`（片段）与 `/admin/xxx/yyy`（接口），两者都不在集合里 → 默认一律放行。本套件已实测：把 `/admin/organization/list` 登记进权限表并重新登录后，访问即被拦为 `000020`。**建议给关键接口补登记权限数据**。
4. 权限变更在**重新登录 / 刷新 token 后**才生效（权限快照在换 token 时生成）——套件已按此设计用例。
5. `login-user.html` 里有一个未被使用的 `organizationId` 隐藏域（历史遗留）。
6. `ClientServerAddressResolver` 的启动提示日志已被移除，`logger` 字段目前空置；功能不受影响，但少了一处可观测性。

## 六、未覆盖 / 后续建议

- **未覆盖**：生产 MySQL profile 的实跑（本机无可用 MySQL，故 prod 仅做配置与语法层面核对）；集群/Redis 模式；浏览器兼容性（仅 Chrome）；压力与并发。
- **建议**：把 `verify/` 纳入 CI（`verify.sh` 与 `front.mjs` 的退出码即结果）；第四节 3 个缺陷的回归用例已就位，后续改动可直接拦住复发。
