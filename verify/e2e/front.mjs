/**
 * L5 前端端到端验证（Playwright + 本机 Chrome）
 *
 * 覆盖：SSO 登录流程、管理台菜单、五个业务页面渲染、
 *       以及本次改造的回归点（父机构默认选中、机构下拉带出、权限树默认勾选、启用/禁用不再弹成功提示）
 *
 * 运行：node front.mjs          （BASE 可用环境变量覆盖，默认 http://127.0.0.1:18080）
 */
import { chromium } from 'playwright-core';
import { mkdirSync } from 'node:fs';

const BASE = process.env.BASE || 'http://127.0.0.1:18080';
const SHOTS = new URL('./screenshots/', import.meta.url).pathname;
mkdirSync(SHOTS, { recursive: true });

const results = [];
const check = (label, ok, detail = '') => {
  results.push({ label, ok, detail });
  console.log(`  ${ok ? 'PASS' : 'FAIL'}  ${label}${detail ? '   [' + detail + ']' : ''}`);
};

const browser = await chromium.launch({ channel: 'chrome', headless: true });
const ctx = await browser.newContext({ viewport: { width: 1600, height: 1000 } });
const page = await ctx.newPage();

const jsErrors = [];
const httpErrors = [];
page.on('pageerror', e => jsErrors.push(String(e).split('\n')[0].slice(0, 140)));
page.on('response', r => {
  if (r.status() >= 400) httpErrors.push(r.status() + ' ' + r.url().replace(BASE, ''));
});

const goto = async hash => {
  await page.goto(BASE + '/' + (hash ? '#' + hash : ''), { waitUntil: 'domcontentloaded' });
  await page.waitForTimeout(1400);
};
const clearToasts = () => page.evaluate(() => document.querySelector('#gritter-notice-wrapper')?.remove());
const toastTexts = () => page.$$eval('.gritter-item-wrapper', els => els.map(e => e.innerText.trim()));
const rowText = i => page.$eval(`#_table tbody tr:nth-child(${i + 1})`, el => el.innerText.replace(/\s+/g, ' '));
const shot = name => page.screenshot({ path: SHOTS + name + '.png', fullPage: true });

// ── L5-1 登录流程 ───────────────────────────────────────────────
console.log('\n【L5-1】SSO 登录流程');
await page.goto(BASE + '/', { waitUntil: 'domcontentloaded' });
check('未登录访问 / 被重定向到登录页', page.url().includes('/login.html'), page.url().replace(BASE, ''));
const loginUrl = new URL(page.url());
check('登录页带上 redirectUri/clientId', loginUrl.searchParams.has('redirectUri') && loginUrl.searchParams.get('clientId') === '1000');
await shot('01-login');
await page.click('#_loginButton');
await page.waitForURL(u => !u.pathname.includes('login.html'), { timeout: 20000 });
await page.waitForTimeout(1500);
check('登录成功后回到管理台（hash 路由）', page.url().startsWith(BASE + '/#/'), page.url().replace(BASE, ''));
check('默认落地页为个人中心', (await page.title()) === '个人中心', await page.title());
await shot('02-home');

// ── L5-2 菜单 ──────────────────────────────────────────────────
console.log('\n【L5-2】管理台菜单');
const menus = await page.$$eval('a[data-url]', els => els.map(e => e.textContent.trim() + '|' + e.getAttribute('data-url')));
const expectMenus = ['个人中心|/profile.html', '机构管理|/organization.html', '用户管理|/user.html',
  '角色管理|/role.html', '应用管理|/app.html', '权限管理|/permission.html', '登录用户管理|/login-user.html'];
check('菜单项数量为 7', menus.length === 7, '实际 ' + menus.length);
check('菜单项与权限数据一致', expectMenus.every(m => menus.includes(m)), JSON.stringify(menus));

// ── L5-3 机构列表 ──────────────────────────────────────────────
console.log('\n【L5-3】机构管理列表');
await goto('/organization.html');
const orgRows = await page.$$eval('#_table tbody tr', r => r.length);
check('机构列表 3 行（种子数据）', orgRows === 3, '实际 ' + orgRows);
const orgNames = await page.$$eval('#_table tbody tr', rs => rs.map(r => r.innerText.replace(/\s+/g, ' ')));
check('机构名称正确（TT公司/XX部门/YY部门）',
  orgNames.join('').includes('TT公司') && orgNames.join('').includes('XX部门') && orgNames.join('').includes('YY部门'));
await shot('03-organization-list');

// ── L5-4 机构编辑：父机构默认选中（回归） ──────────────────────
console.log('\n【L5-4】机构编辑（回归：父机构默认选中）');
await goto('/organization/edit?id=2');
await page.waitForFunction(() => document.querySelector('#_name')?.value === 'XX部门', null, { timeout: 8000 }).catch(() => {});
check('编辑页带出名称', (await page.inputValue('#_name')) === 'XX部门', await page.inputValue('#_name'));
check('编辑页带出排序', (await page.inputValue('#_sort')) === '30', await page.inputValue('#_sort'));
const parent = await page.$eval('#_parentId', el => ({ value: el.value, text: el.selectedOptions[0]?.textContent ?? '' }));
check('父机构默认选中（=TT公司）', parent.value === '1' && parent.text.includes('TT公司'), JSON.stringify(parent));
await shot('04-organization-edit');

// ── L5-5 启用/禁用不再弹成功提示（回归 + 检测器对照） ──────────
console.log('\n【L5-5】启用/禁用提示收敛');
await goto('/organization.html');
await clearToasts();
await page.click('#_tr1');                       // 工具栏「禁用」，未选行 → 应提示
await page.waitForTimeout(900);
const warn = await toastTexts();
check('对照组：未选行点工具栏禁用有提示（证明提示检测有效）', warn.some(t => t.includes('至少选择一行')), JSON.stringify(warn));
await clearToasts();
await page.click('#_mr0c1');                     // 行内「禁用」
await page.waitForTimeout(3000);
const afterDisable = await toastTexts();
check('行内禁用：成功后无任何弹出提示', afterDisable.length === 0, JSON.stringify(afterDisable));
check('行内禁用：列表状态已变为「否」', (await rowText(0)).includes('否'), await rowText(0));
await clearToasts();
await page.click('#_mr0c2');                     // 行内「启用」恢复现场
await page.waitForTimeout(3000);
const afterEnable = await toastTexts();
check('行内启用：成功后无任何弹出提示', afterEnable.length === 0, JSON.stringify(afterEnable));
check('行内启用：列表状态已恢复「是」', (await rowText(0)).includes('是'), await rowText(0));
await shot('05-organization-after-enable');

// ── L5-6 删除仍保留确认框 ─────────────────────────────────────
console.log('\n【L5-6】删除确认框（保留项）');
await page.click('#_mr0c3');                     // 行内「删除」
await page.waitForTimeout(900);
const dialog = await page.$$eval('.bootbox-body', els => els.map(e => e.innerText.trim()));
check('删除仍弹出确认框', dialog.length > 0, JSON.stringify(dialog));
await shot('06-delete-confirm');
if (dialog.length) await page.click('.bootbox .btn-default');   // 取消
await page.waitForTimeout(800);
check('取消后数据未变（仍 3 行）', (await page.$$eval('#_table tbody tr', r => r.length)) === 3);

// ── L5-7 用户编辑：机构下拉带出（回归） ────────────────────────
console.log('\n【L5-7】用户编辑（回归：机构下拉带出）');
await goto('/user/edit?id=2');
await page.waitForFunction(() => (document.querySelector('#_organizationId')?.options.length ?? 0) > 1, null, { timeout: 8000 }).catch(() => {});
const orgSel = await page.$eval('#_organizationId', el => ({ value: el.value, text: el.selectedOptions[0]?.textContent ?? '', n: el.options.length }));
check('机构下拉已加载（1 个占位 + 3 个机构）', orgSel.n === 4, 'options=' + orgSel.n);
check('机构下拉默认选中用户所属机构', orgSel.value === '3' && orgSel.text.includes('YY部门'), JSON.stringify(orgSel));
check('登录名带出', (await page.inputValue('#_account')) === 'admin', await page.inputValue('#_account'));
await shot('07-user-edit');

// ── L5-8 角色授权：权限树默认勾选（回归） ──────────────────────
console.log('\n【L5-8】角色授权（回归：已授权权限默认勾选）');
await goto('/role-permission?roleId=1');
await page.waitForFunction(() => (document.querySelectorAll('.ztree li').length ?? 0) > 1, null, { timeout: 10000 }).catch(() => {});
const roleName = await page.inputValue('#_name');
const nodes = await page.$$eval('.ztree li', l => l.length);
// zTree 的复选框是样式化的 span（非真实 input），状态在 class 名里；同时用 zTree API 复核
const checked = await page.evaluate(() => ({
  span: document.querySelectorAll('.ztree li span.checkbox_true_full').length,
  api: window.jQuery.fn.zTree.getZTreeObj('_tree')?.getCheckedNodes(true).length ?? -1
}));
check('当前角色带出（系统管理员）', roleName === '系统管理员', roleName);
check('权限树已渲染', nodes > 1, '节点=' + nodes);
check('已授权权限默认勾选', checked.span > 0 && checked.api > 0, JSON.stringify(checked));
await shot('08-role-permission');

// ── L5-9 权限管理树 + 应用下拉 ─────────────────────────────────
console.log('\n【L5-9】权限管理');
await goto('/permission.html');
await page.waitForTimeout(1200);
const permNodes = await page.$$eval('.ztree li', l => l.length);
const apps = await page.$$eval('#_searchAppId option', o => o.map(x => x.textContent.trim()));
check('权限树渲染', permNodes > 1, '节点=' + permNodes);
check('应用下拉加载（服务器 + demo）', apps.length === 2, JSON.stringify(apps));

// 回归：权限树必须返回 isMenu/url 等业务字段，否则页面“添加子权限”按钮不会出现
const treeInfo = await page.evaluate(() => {
  const zt = window.jQuery.fn.zTree.getZTreeObj('_tree');
  const flat = []; const walk = ns => ns.forEach(n => { flat.push(n); walk(n.children || []); });
  walk(zt ? zt.getNodes() : []);
  const menu = flat.find(n => n.isMenu === true);
  return { total: flat.length, hasIsMenu: flat.some(n => n.isMenu !== undefined),
           noIconNoUrl: flat.every(n => n.icon === undefined && n.url === undefined),
           menu: menu ? { id: menu.id, name: menu.name, tId: menu.tId } : null };
});
check('权限树节点带 isMenu 字段（缺陷3 回归）', treeInfo.hasIsMenu, 'hasIsMenu=' + treeInfo.hasIsMenu);
check('权限树不暴露 icon/url（避免 zTree 当图片路径与超链接）', treeInfo.noIconNoUrl, 'noIconNoUrl=' + treeInfo.noIconNoUrl);
if (treeInfo.menu) {
  await page.hover('#' + treeInfo.menu.tId + '_a');
  await page.waitForTimeout(700);
  const addBtns = await page.$$eval('a[id^=add_]', a => a.length);
  check('菜单节点悬停出现「添加子权限」按钮（缺陷3 回归）', addBtns > 0,
    '菜单节点=' + treeInfo.menu.name + ' add按钮=' + addBtns);
} else {
  check('菜单节点悬停出现「添加子权限」按钮（缺陷3 回归）', false, '未找到 isMenu=true 的节点');
}
await shot('09-permission');

// ── L5-10 全程无 JS 错误 / 无 4xx ──────────────────────────────
console.log('\n【L5-10】健康性');
const uniqJs = [...new Set(jsErrors)];
const uniqHttp = [...new Set(httpErrors)].filter(u => !u.includes('favicon'));
check('无 JS 运行错误', uniqJs.length === 0, JSON.stringify(uniqJs.slice(0, 3)));
check('无 4xx/5xx 资源请求', uniqHttp.length === 0, JSON.stringify(uniqHttp.slice(0, 4)));

await browser.close();

const failed = results.filter(r => !r.ok);
console.log(`\n结果：${results.length - failed.length}/${results.length} 通过${failed.length ? '，失败 ' + failed.length + ' 项' : ''}`);
if (failed.length) { failed.forEach(f => console.log('  ✗ ' + f.label)); process.exit(1); }
