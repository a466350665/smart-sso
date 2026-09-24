#!/usr/bin/env bash
#
# Smart-SSO 服务端验证
#   L0 构建与启动 / L1 SSO 协议主链路 / L4 安全与权限 / L2 只读接口全量 / L3 业务 CRUD
#
# 用法：
#   verify/verify.sh                                            # 构建 + 启动(dev/H2) + 验证 + 关闭
#   verify/verify.sh --no-build                                 # 复用已有 jar
#   verify/verify.sh --external --base http://127.0.0.1:18090   # 对已运行实例验证（不构建不启停）
#
set -uo pipefail

PORT=18080; DO_BUILD=1; KEEP=0; EXTERNAL=0; BASE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --no-build) DO_BUILD=0;;
    --keep) KEEP=1;;
    --external) EXTERNAL=1;;
    --port) PORT="$2"; shift;;
    --base) BASE="$2"; shift;;
    *) echo "未知参数: $1"; exit 2;;
  esac
  shift
done
[ -n "$BASE" ] || BASE="http://127.0.0.1:$PORT"

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JAR="$ROOT/smart-sso-server/target/smart-sso-server-2.0.1.jar"
COOKIE="/tmp/sso-cookie.$$"; LOG="/tmp/sso-app.$$.log"; APP_PID=""
PASS=0; FAIL=0; XFAIL=0

green(){ printf '\033[32m%s\033[0m' "$1"; }; red(){ printf '\033[31m%s\033[0m' "$1"; }
pass(){ PASS=$((PASS+1)); echo "  $(green PASS)  $1"; }
fail(){ FAIL=$((FAIL+1)); echo "  $(red FAIL)  $1${2:+   [$2]}"; }
xfail(){ XFAIL=$((XFAIL+1)); printf "  \033[33m%s\033[0m  %s%s\n" XFAIL "$1" "${2:+   [$2]}"; }
section(){ echo; echo "【$1】"; }
cleanup(){ if [ "$EXTERNAL" = 0 ] && [ -n "$APP_PID" ]; then kill "$APP_PID" 2>/dev/null; fi; rm -f "$COOKIE"; }
trap cleanup EXIT

jqr(){ jq -r "$1" 2>/dev/null <<<"$2"; }
get(){ curl -s -b "$COOKIE" -c "$COOKIE" "$BASE$1"; }
post(){ curl -s -b "$COOKIE" -c "$COOKIE" -X POST "$BASE$1" "${@:2}"; }
status(){ curl -s -o /dev/null -w '%{http_code}' -b "$COOKIE" -c "$COOKIE" "$BASE$1"; }
location(){ curl -s -D - -o /dev/null -b "$COOKIE" -c "$COOKIE" "$BASE$1" | tr -d '\r' | awk 'tolower($1)=="location:"{print $2}'; }

login(){ local enc; enc="$(jq -rn --arg u "$BASE/admin/admin.html" '$u|@uri')"
  rm -f "$COOKIE"
  curl -s -c "$COOKIE" "$BASE/sso/login?clientId=1000&redirectUri=$enc" -o /dev/null
  local res; res="$(post /sso/login -d "redirectUri=$enc&clientId=1000&username=admin&password=123456")"
  curl -s -b "$COOKIE" -c "$COOKIE" "$(jqr '.data' "$res" | tr -d '\r\n')" -o /dev/null; }

assert_code(){ local l="$1" e="$2" j="$3" c
  [ -z "$j" ] && { fail "$l" "空响应（服务不可达？）"; return; }
  c="$(jqr '.code' "$j")"
  [ "$c" = "$e" ] && pass "$l" || fail "$l" "期望 code=$e 实际=${c:-无} body=$(head -c 120 <<<"$j")"; }
assert_absent(){ local l="$1" j="$2" needle="$3"
  [ -z "$j" ] && { fail "$l" "空响应"; return; }
  if [[ "$j" == *"$needle"* ]]; then fail "$l" "仍存在 $needle"; else pass "$l"; fi; }
assert_present(){ local l="$1" j="$2" needle="$3"
  [ -z "$j" ] && { fail "$l" "空响应"; return; }
  if [[ "$j" == *"$needle"* ]]; then pass "$l"; else fail "$l" "缺少 $needle"; fi; }

section "L0 构建与启动"
if [ "$EXTERNAL" = 0 ]; then
  if [ "$DO_BUILD" = 1 ]; then
    if (cd "$ROOT" && mvn -o -q -DskipTests clean package >/tmp/verify-build.log 2>&1); then
      pass "mvn -o -DskipTests clean package"
    else fail "构建失败" "见 /tmp/verify-build.log"; exit 1; fi
  fi
  [ -f "$JAR" ] && pass "产物存在" || { fail "缺少 $JAR"; exit 1; }
  java -jar "$JAR" --server.port="$PORT" >"$LOG" 2>&1 &
  APP_PID=$!
  for i in $(seq 1 40); do grep -q "Started ServerApplication" "$LOG" 2>/dev/null && break; sleep 1; done
  if grep -q "Started ServerApplication" "$LOG"; then pass "应用启动成功（端口 ${PORT}）"
  else fail "启动超时"; tail -20 "$LOG"; exit 1; fi
  grep -q 'profile is active: "dev"' "$LOG" && pass "默认 profile = dev" || fail "profile 不是 dev"
  grep -q "jdbc:h2:mem:smart_sso" "$LOG" && pass "数据源 = H2 内存库（开箱即用）" || fail "未使用 H2"
else
  if [ "$(curl -s -o /dev/null -w '%{http_code}' "$BASE/")" != "000" ]; then pass "外部实例可达 $BASE"; else fail "外部实例不可达 $BASE"; exit 1; fi
fi

section "L1 SSO 协议主链路"
[ "$(status /admin/admin.html)" = "302" ] && pass "未登录访问受保护页面 -> 302" || fail "未登录未跳转（$(status /admin/admin.html)）"
if [[ "$(location /admin/admin.html)" == *"/sso/login"* ]]; then pass "跳转 /sso/login（同源相对路径）"; else fail "登录地址异常"; fi

RDU="$BASE/admin/admin.html"; RDU_ENC="$(jq -rn --arg u "$RDU" '$u|@uri')"
if [[ "$(location "/sso/login?clientId=1000&redirectUri=$RDU_ENC")" == *"/login.html"* ]]; then pass "无 TGT 时跳登录页 /login.html"; else fail "未跳登录页"; fi

LOGIN="$(post /sso/login -d "redirectUri=$RDU_ENC&clientId=1000&username=admin&password=123456")"
assert_code "POST /sso/login 登录成功" 000000 "$LOGIN"
CB="$(jqr '.data' "$LOGIN" | tr -d '\r\n')"
assert_present "登录返回带 code 的回跳地址" "$CB" "code="

CS="$(curl -s -o /dev/null -w '%{http_code}' -b "$COOKIE" -c "$COOKIE" "$CB")"
[ "$CS" = "302" ] && pass "客户端用 code 换 token 后回跳（同源内部自调用成功）" || fail "callback 状态 $CS"
grep -q "smart-sso-token-1000" "$COOKIE" && pass "下发 accessToken Cookie" || fail "未下发 token cookie"

UI="$(get /admin/admin/userinfo)"
assert_code "带 token 访问 /admin/admin/userinfo" 000000 "$UI"
if [[ "$(jqr '.data.logoutUrl' "$UI")" == /sso/logout* ]]; then pass "logoutUrl 为相对路径（同源模式生效）"; else fail "logoutUrl=$(jqr '.data.logoutUrl' "$UI")"; fi
if [[ "$(jqr '.data' "$(get '/auth/login_url?redirectUri=http%3A%2F%2Fx%2Fy')")" == /sso/login* ]]; then pass "H5 入口 /auth/login_url 返回相对地址"; else fail "H5 登录地址异常"; fi

if [[ "$(location "/sso/logout?redirectUri=$RDU_ENC")" == *"/admin/admin.html"* ]]; then pass "登出回跳到 redirectUri"; else fail "登出回跳异常"; fi
sleep 1
AT="$(jqr '.code' "$(get /admin/admin/userinfo)")"
[ "$AT" != "000000" ] && pass "登出后原 token 失效（${AT:-重定向}）" || fail "登出后 token 仍有效"

section "L4 安全与权限"
rm -f "$COOKIE"
assert_code "未登录 + AJAX -> 000010（JSON 而非重定向）" 000010 "$(curl -s -H 'X-Requested-With: XMLHttpRequest' "$BASE/admin/app/all")"
[ "$(status /admin/app/all)" = "302" ] && pass "未登录 + 普通请求 -> 302 跳登录" || fail "状态 $(status /admin/app/all)"

curl -s -c "$COOKIE" "$BASE/sso/login?clientId=1000&redirectUri=$RDU_ENC" -o /dev/null
L2="$(post /sso/login -d "redirectUri=$RDU_ENC&clientId=1000&username=admin&password=123456")"
curl -s -b "$COOKIE" -c "$COOKIE" "$(jqr '.data' "$L2" | tr -d '\r\n')" -o /dev/null
assert_code "重新登录成功" 000000 "$(get /admin/admin/userinfo)"

assert_code "登记一条未被授权的接口 URL 权限（/admin/organization/list）" 000000 \
  "$(post /admin/permission/save -d 'appId=1&name=验证用-接口权限&url=/admin/organization/list&sort=1&isMenu=false&isEnable=true')"
login   # 权限快照在登录/换 token 时生成，必须重新登录才生效
assert_code "重新登录后：访问未授权接口 -> 000020 无权限" 000020 "$(get '/admin/organization/list?current=1&size=10')"
for TMPID in $(get '/admin/permission/tree?appId=1&isEnable=true' | jq -r '.. | objects | select((.name // "")=="验证用-接口权限") | .id'); do
  post /admin/permission/delete -d "id=$TMPID&appId=1" -o /dev/null
done
login
assert_code "删除该权限、重新登录后恢复访问" 000000 "$(get '/admin/organization/list?current=1&size=10')"

section "L2 只读接口全量"
while IFS='|' read -r label path exp; do
  [ -z "$label" ] && continue
  J="$(get "$path")"
  if [ "$exp" = "RAW" ]; then
    N="$(jq 'length' <<<"$J" 2>/dev/null)"; N="${N:-0}"
    if [ -z "$J" ]; then fail "$label" "空响应"
    elif [ "$N" -ge 1 ]; then pass "$label"
    else fail "$label" "空列表: $(head -c 80 <<<"$J")"; fi
  else assert_code "$label" "$exp" "$J"; fi
done <<'EOF'
GET /admin/admin/menu|/admin/admin/menu|000000
GET /admin/app/all|/admin/app/all|000000
GET /admin/app/list|/admin/app/list?current=1&size=10|000000
GET /admin/app/credentials|/admin/app/credentials?id=1|000000
GET /admin/organization/list|/admin/organization/list?current=1&size=10|000000
GET /admin/organization/all|/admin/organization/all|000000
GET /admin/organization/get|/admin/organization/get?id=1|000000
GET /admin/role/list|/admin/role/list?current=1&size=10|000000
GET /admin/user/list|/admin/user/list?current=1&size=10|000000
GET /admin/user/get|/admin/user/get?id=2|000000
GET /admin/user-role/roles|/admin/user-role/roles?userId=2|000000
GET /admin/permission/get|/admin/permission/get?id=2|000000
GET /admin/permission/tree|/admin/permission/tree?appId=1&isEnable=true|RAW
GET /admin/user/organization/tree|/admin/user/organization/tree|RAW
EOF
LJ="$(get '/admin/login-user/list?current=1&size=10')"; LJC="$(jqr '.code' "$LJ")"
if [ "$LJC" = "000000" ]; then pass "GET /admin/login-user/list"; else xfail "GET /admin/login-user/list（已知缺陷：getClientIdMapByTgt NPE）" "code=$LJC"; fi
assert_code "POST /admin/app/validate-code" 000000 "$(post /admin/app/validate-code -d "code=verify-tmp-$$")"
assert_code "POST /admin/user/validate-account" 000000 "$(post /admin/user/validate-account -d "account=verify-tmp-$$")"
assert_code "POST /admin/profile/save-password（原值回写）" 000000 "$(post /admin/profile/save-password -d 'newPassword=123456')"

section "L3 业务功能 CRUD"
post /admin/organization/save -d 'parentId=1&name=验证机构A&sort=9&isEnable=true' -o /dev/null
OID="$(get /admin/organization/all | jq -r '.data[] | select(.name|contains("验证机构A")) | .id' | head -1)"
[ -n "$OID" ] && pass "机构：新增成功（id=${OID}）" || fail "机构：新增后查不到"
post /admin/organization/save -d "id=$OID&parentId=1&name=验证机构A2&sort=8&isEnable=true" -o /dev/null
assert_present "机构：修改名称生效" "$(get /admin/organization/all)" "验证机构A2"
post /admin/organization/enable -d "ids=$OID&isEnable=false" -o /dev/null
[ "$(get '/admin/organization/list?current=1&size=50' | jq -r --arg id "${OID:-0}" '.data[] | select(.id==($id|tonumber)) | .isEnable')" = "false" ] && pass "机构：禁用生效（注：/organization/all 只返回启用项）" || fail "机构：禁用未生效"
post /admin/organization/delete -d "ids=$OID" -o /dev/null
assert_absent "机构：删除生效" "$(get /admin/organization/all)" "验证机构A2"

post /admin/app/save -d 'name=验证应用&code=verify-app&isEnable=true&sort=1' -o /dev/null
AID="$(get '/admin/app/list?current=1&size=50' | jq -r '.data.records[] | select(.code=="verify-app") | .id' | head -1)"
[ -n "$AID" ] && pass "应用：新增成功（id=${AID}，自动生成 clientId/secret）" || fail "应用：新增后查不到"
[ -n "$(get "/admin/app/credentials?id=${AID:-0}" | jq -r '.data.clientId')" ] && pass "应用：密钥可查询" || fail "应用：密钥查询失败"
post /admin/app/enable -d "ids=$AID&isEnable=false" -o /dev/null
[ "$(get '/admin/app/list?current=1&size=50' | jq -r --arg id "${AID:-0}" '.data.records[] | select(.id==($id|tonumber)) | .isEnable')" = "false" ] && pass "应用：禁用生效" || fail "应用：禁用未生效"
post /admin/app/delete -d "ids=$AID" -o /dev/null
assert_absent "应用：删除生效" "$(get '/admin/app/list?current=1&size=50')" "verify-app"

post /admin/role/save -d 'name=验证角色&sort=1&description=verify&isEnable=true' -o /dev/null
RID="$(get '/admin/role/list?current=1&size=50' | jq -r '.data.records[] | select(.name=="验证角色") | .id' | head -1)"
[ -n "$RID" ] && pass "角色：新增成功（id=${RID}）" || fail "角色：新增后查不到"
assert_code "角色：授权 2 个权限" 000000 "$(post /admin/role-permission/save -d "appId=1&roleId=$RID&permissionIds=2,3")"
CHK="$(get "/admin/permission/tree?appId=1&roleId=${RID:-0}&isEnable=true" | jq '[.. | objects | select(.checked==true)] | length')"; CHK="${CHK:-0}"
[ "$CHK" -ge 2 ] && pass "角色：权限树回显已授权（checked=${CHK}）" || fail "角色：授权回显异常（checked=${CHK}）"
assert_code "角色：分配给用户" 000000 "$(post /admin/user-role/save -d "userId=2&roleIds=1,$RID")"
ROLES="$(get '/admin/user-role/roles?userId=2')"
[ "$(jq -r --arg id "${RID:-0}" '.data[] | select(.id==($id|tonumber)) | .checked' <<<"$ROLES")" = "true" ] && pass "角色：用户角色列表回显已勾选" || fail "角色：用户角色回显异常"
post /admin/role/delete -d "ids=$RID" -o /dev/null
assert_absent "角色：删除生效（级联清理授权/关联）" "$(get '/admin/role/list?current=1&size=50')" "验证角色"
post /admin/user-role/save -d 'userId=2&roleIds=1' -o /dev/null

post /admin/user/save -d 'organizationId=1&name=验证用户&account=verifyuser&password=123456&isEnable=true' -o /dev/null
VUID="$(get '/admin/user/list?current=1&size=50' | jq -r '.data.records[] | select(.account=="verifyuser") | .id' | head -1)"
[ -n "$VUID" ] && pass "用户：新增成功（id=${VUID}）" || fail "用户：新增后查不到"
post /admin/user/enable -d "ids=$VUID&isEnable=false" -o /dev/null
[ "$(get '/admin/user/list?current=1&size=50' | jq -r --arg id "${VUID:-0}" '.data.records[] | select(.id==($id|tonumber)) | .isEnable')" = "false" ] && pass "用户：禁用生效" || fail "用户：禁用未生效"
assert_code "用户：重置密码" 000000 "$(post /admin/user/reset-password -d "ids=$VUID")"
post /admin/user/delete -d "ids=$VUID" -o /dev/null
assert_absent "用户：删除生效" "$(get '/admin/user/list?current=1&size=50')" "verifyuser"

post /admin/permission/save -d 'appId=1&name=验证权限&url=/verify.html&sort=1&isMenu=false&isEnable=true' -o /dev/null
PID2="$(get '/admin/permission/tree?appId=1&isEnable=true' | jq -r '.. | objects | select((.name // "")=="验证权限") | .id' | head -1)"
[ -n "$PID2" ] && pass "权限：新增成功（id=${PID2}）" || fail "权限：新增后查不到"
post /admin/permission/delete -d "id=$PID2&appId=1" -o /dev/null
assert_absent "权限：删除生效" "$(get '/admin/permission/tree?appId=1&isEnable=true')" "验证权限"

section "缺陷回归（原 3 个缺陷对应用例）"
# 缺陷2：一次未走完回跳（没换到 token）的登录，会让“登录用户管理”页面持续报错
ENC="$(jq -rn --arg u "$BASE/admin/admin.html" '$u|@uri')"
rm -f "$COOKIE"
curl -s -c "$COOKIE" "$BASE/sso/login?clientId=1000&redirectUri=$ENC" -o /dev/null
post /sso/login -d "redirectUri=$ENC&clientId=1000&username=admin&password=123456" -o /dev/null   # 只产生 TGT，不访问 callback
login                                                                                            # 再建一个正常会话
LJ2="$(get '/admin/login-user/list?current=1&size=10')"; LJ2C="$(jqr '.code' "$LJ2")"
assert_code "存在“未完成换 token”的登录时，/admin/login-user/list 仍正常（缺陷2 回归）" 000000 "$LJ2"

assert_code "空 ids 的启用/禁用被安全处理（缺陷1 回归）" 000000 "$(post /admin/user/enable -d 'ids=&isEnable=false')"
assert_code "空 ids 的删除被安全处理（缺陷1 回归）" 000000 "$(post /admin/user/delete -d 'ids=')"
assert_code "空 ids 的机构禁用被安全处理（缺陷1 回归）" 000000 "$(post /admin/organization/enable -d 'ids=&isEnable=false')"
assert_code "空 ids 的机构删除被安全处理（缺陷1 回归）" 000000 "$(post /admin/organization/delete -d 'ids=')"
assert_code "空 ids 的角色禁用被安全处理（缺陷1 回归）" 000000 "$(post /admin/role/enable -d 'ids=&isEnable=false')"
assert_code "空 ids 的应用删除被安全处理（缺陷1 回归）" 000000 "$(post /admin/app/delete -d 'ids=')"

echo; echo "════════════════════════════════════════"
echo " 通过 $PASS 项，失败 $FAIL 项，已知缺陷 $XFAIL 项"
if [ "$EXTERNAL" = 0 ]; then if [ "$KEEP" = 1 ]; then echo " 应用仍在运行：$BASE"; else echo " 应用已停止"; fi; fi
echo "════════════════════════════════════════"
[ "$FAIL" -eq 0 ] || exit 1
