/**
 * 用真实浏览器截取 README 用的界面图（只浏览、不改数据）
 * 运行：BASE=http://127.0.0.1:18094 node screenshots.mjs
 * 输出：../../images/*.png
 */
import { chromium } from 'playwright-core';
import { mkdirSync } from 'node:fs';

const BASE = process.env.BASE || 'http://127.0.0.1:18094';
const OUT = new URL('../../images/', import.meta.url).pathname;
mkdirSync(OUT, { recursive: true });

const browser = await chromium.launch({ channel: 'chrome', headless: true });
const ctx = await browser.newContext({ viewport: { width: 1440, height: 900 }, deviceScaleFactor: 2 });
const page = await ctx.newPage();
const wait = (ms) => page.waitForTimeout(ms);

// 1) 登录页（未登录访问受保护页会被引导到这里）
await page.goto(BASE + '/', { waitUntil: 'domcontentloaded' });
await page.waitForTimeout(1200);
await page.screenshot({ path: OUT + 'admin-login.png' });
console.log('  admin-login.png');

// 2) 登录进入管理台
await page.click('#_loginButton');
await page.waitForURL(u => !u.pathname.includes('login.html'), { timeout: 20000 });
await page.waitForTimeout(1800);
await page.screenshot({ path: OUT + 'admin-home.png' });
console.log('  admin-home.png');

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
  await page.screenshot({ path: OUT + file });
  console.log('  ' + file);
}

await browser.close();
console.log('截图完成 → images/');
