# verify —— Smart-SSO 功能验证套件

针对 `smart-sso-server` 的真实运行验证：真实构建 → 真实启动（dev/H2）→ 真实 HTTP / 真实浏览器。

## 目录

```
verify/
├── verify.sh          # L0 构建启动 + L1 协议 + L4 安全权限 + L2 接口 + L3 业务 CRUD
├── e2e/
│   ├── package.json   # 依赖 playwright-core（复用本机 Chrome，不下载浏览器）
│   ├── front.mjs      # L5 前端端到端（登录、菜单、业务页、权限树、弹窗收敛）
│   └── screenshots/   # 运行时截图（已 gitignore）
└── REPORT.md          # 本次验证结果与缺陷清单
```

## 运行

```bash
# 1) 一次性：构建 + 启动 + 全量断言 + 关闭（会把结果打印到 stdout，退出码即结论）
verify/verify.sh

# 2) 常用：复用已启动实例（自己用 IDE / java -jar 起的都行）
java -jar smart-sso-server/target/smart-sso-server-2.0.1.jar --server.port=18090 &
verify/verify.sh --external --base http://127.0.0.1:18090
```

```bash
# 3) 前端 L5：需要本机已装 Google Chrome
cd verify/e2e
npm install                       # 首次
BASE=http://127.0.0.1:18090 node front.mjs
```

退出码：`0` 全部通过（含已知缺陷 XFAIL）；非 0 表示有断言失败。

## 参数

| 参数 | 说明 |
|---|---|
| `--no-build` | 跳过 `mvn`，直接复用已有 jar |
| `--keep` | 验证结束后不关闭应用（便于手动排查） |
| `--port` | 自启应用使用的端口（默认 18080） |
| `--external --base <url>` | 对已运行实例验证，不构建、不启停 |

## 覆盖范围

- **L0** 构建、dev profile、H2 内存库、启动
- **L1** SSO：未登录跳转 → 登录页 → 换 code → 下发 token Cookie → 登出 → H5 入口
- **L4** 安全：未登录（302 / 000010）、权限拦截（000020）、权限变更生效时机
- **L2** 只读接口全量（含校验类接口）
- **L3** 业务 CRUD：应用 / 机构 / 角色（含授权与分配）/ 用户 / 权限，含级联删除；数据自建自清
- **L5** 前端：登录流程、7 个菜单、机构/用户/角色/权限页渲染、机构编辑父机构默认选中、权限树默认勾选、启用禁用无成功提示、删除确认框
- **边界** 见 `REPORT.md` 第三节（token 过期后 AJAX 与整页两条分支，需用 `--smart.sso.server.access-token-timeout=1` 启动实例验证）

## 说明

- 断言依赖种子账号 `admin / 123456`（H2 每次启动按 `resources/db/smart-sso.sql` 重建），因此**请对 dev/H2 实例运行**，不要指向生产库。
- `L3` 会创建并删除测试数据，跑完自动清理；中断可能留下 `验证*` 前缀的数据，重启实例即可（H2 内存库）。
- 已知缺陷会打印为 `XFAIL`（黄色）并计入统计，不计入失败：详见 `REPORT.md` 第四节。
