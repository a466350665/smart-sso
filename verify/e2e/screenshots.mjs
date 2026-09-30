/**
 * 用真实浏览器截取 README 用的界面图（只浏览、不改数据）
 * 运行：BASE=http://127.0.0.1:18094 node screenshots.mjs
 *       ONLY=admin-login.png ...                       # 只重新生成文件名包含该子串的图（可用逗号分隔多个）
 * 输出：../../docs/images/*.png
 */
import { chromium } from 'playwright-core';
import { mkdirSync } from 'node:fs';

const BASE = process.env.BASE || 'http://127.0.0.1:18094';
const OUT = new URL('../../docs/images/', import.meta.url).pathname;
const ONLY = (process.env.ONLY || '').split(',').map(s => s.trim()).filter(Boolean);
const want = (file) => ONLY.length === 0 || ONLY.some(k => file.includes(k));
mkdirSync(OUT, { recursive: true });

const browser = await chromium.launch({ channel: 'chrome', headless: true });
const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, deviceScaleFactor: 2 });
const page = await ctx.newPage();
const wait = (ms) => page.waitForTimeout(ms);
const shoot = async (file) => {
  if (!want(file)) return;
  await page.screenshot({ path: OUT + file });
  console.log('  ' + file);
};

// 1) 登录页（未登录访问受保护页会被引导到这里）
await page.goto(BASE + '/', { waitUntil: 'domcontentloaded' });
await page.waitForTimeout(1200);
await shoot('admin-login.png');

// 2) 登录进入管理台
await page.click('#_loginButton');
await page.waitForURL(u => !u.pathname.includes('login.html'), { timeout: 20000 });
await page.waitForTimeout(1800);
await shoot('admin-home.png');

const shots = [
  ['/organization.html', 'admin-organization.png', 1800],
  ['/organization/edit?id=2', 'admin-organization-edit.png', 2200],
  ['/user.html', 'admin-user.png', 2200],
  ['/role.html', 'admin-role.png', 1600],
  ['/role-permission?roleId=1', 'admin-role-permission.png', 3000],
  ['/permission.html', 'admin-permission.png', 2600],
  ['/app.html', 'admin-app.png', 1600],
  ['/login-user.html', 'admin-login-user.png', 1600],
];
for (const [hash, file, ms] of shots) {
  await page.goto(BASE + '/#' + hash, { waitUntil: 'domcontentloaded' });
  await wait(ms);
  await shoot(file);
}

await browser.close();
console.log('截图完成 → docs/images/' + (ONLY ? `（仅 ${ONLY}）` : ''));
