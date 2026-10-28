-- Align existing MySQL permission codes with the backend routes used by PermissionFilter.
-- The seed system administrator is role_id=1. Existing route permission IDs are retained
-- so current role assignments continue to apply to the routes they previously represented.

SET AUTOCOMMIT FALSE;

UPDATE `sso_permission` SET `name`='应用管理', `code`='/admin/app.html' WHERE `id`=2 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='用户管理', `code`='/admin/user.html' WHERE `id`=3 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='角色管理', `code`='/admin/role.html' WHERE `id`=4 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='权限管理', `code`='/admin/permission.html' WHERE `id`=5 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/app/enable' WHERE `id`=7 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/app/delete' WHERE `id`=9 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/user/enable' WHERE `id`=11 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/user/delete' WHERE `id`=13 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/user/reset-password' WHERE `id`=14 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/role/enable' WHERE `id`=17 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/role/delete' WHERE `id`=19 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='角色授权页', `code`='/admin/role-permission.html' WHERE `id`=20 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/app/list' WHERE `id`=22 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/user/list' WHERE `id`=23 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/role/list' WHERE `id`=24 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/permission/tree' WHERE `id`=25 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='应用编辑', `code`='/admin/app/save' WHERE `id`=26 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='用户编辑', `code`='/admin/user/save' WHERE `id`=27 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='角色编辑', `code`='/admin/role/save' WHERE `id`=28 AND `app_id`=1;
UPDATE `sso_permission` SET `name`='权限编辑', `code`='/admin/permission/save' WHERE `id`=29 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/permission/delete' WHERE `id`=30 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/admin/menu' WHERE `id`=39 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/profile.html' WHERE `id`=41 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/profile/save-password' WHERE `id`=42 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/organization.html' WHERE `id`=60 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/organization/list' WHERE `id`=61 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/organization/enable' WHERE `id`=63 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/organization/delete' WHERE `id`=65 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/login-user.html' WHERE `id`=66 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/login-user/list' WHERE `id`=67 AND `app_id`=1;
UPDATE `sso_permission` SET `code`='/admin/login-user/logout' WHERE `id`=68 AND `app_id`=1;

-- The editor pages are static UI resources. Their submit buttons and the
-- corresponding list actions are controlled by the save API permissions below,
-- so the old page-only permission records are no longer needed.
DELETE FROM `sso_role_permission` WHERE `permission_id` IN (6,10,16,62) AND `app_id`=1;
DELETE FROM `sso_permission` WHERE `id` IN (6,10,16,62) AND `app_id`=1;

INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,NULL,'管理台入口','/',100,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,41,'当前用户信息','/admin/admin/userinfo',98,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/admin/userinfo');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,2,'查看应用密钥','/admin/app/credentials',2,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/app/credentials');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,2,'全部启用应用','/admin/app/all',4,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/app/all');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,2,'验证应用编码','/admin/app/validate-code',3,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/app/validate-code');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,60,'全部启用机构','/admin/organization/all',4,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/organization/all');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,60,'机构信息','/admin/organization/get',3,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/organization/get');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,60,'机构编辑','/admin/organization/save',2,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/organization/save');
UPDATE `sso_permission` SET `name`='机构编辑' WHERE `app_id`=1 AND `code`='/admin/organization/save';
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,5,'权限信息','/admin/permission/get',3,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/permission/get');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,4,'角色授权提交','/admin/role-permission/save',1,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/role-permission/save');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'用户信息','/admin/user/get',5,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user/get');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'验证登录名','/admin/user/validate-account',4,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user/validate-account');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'用户机构树','/admin/user/organization/tree',3,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user/organization/tree');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'分配用户角色页','/admin/user-role.html',2,'fa-cog grey','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user-role.html');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'用户角色列表','/admin/user-role/roles',1,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user-role/roles');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,3,'分配用户角色','/admin/user-role/save',1,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/user-role/save');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 1,NULL,'管理台内容页','/admin/admin.html',99,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=1 AND `code`='/admin/admin.html');

-- /admin/admin.html replaces the removed server /index.html. Preserve any
-- existing custom-role grants before deleting the obsolete permission.
INSERT INTO `sso_role_permission` (`role_id`,`permission_id`,`app_id`)
SELECT DISTINCT rp.`role_id`,new_permission.`id`,1
FROM `sso_role_permission` rp
JOIN `sso_permission` old_permission
  ON old_permission.`id`=rp.`permission_id`
 AND old_permission.`app_id`=1
JOIN `sso_permission` new_permission
  ON new_permission.`app_id`=1
 AND new_permission.`code`='/admin/admin.html'
WHERE old_permission.`code`='/index.html'
  AND NOT EXISTS (
    SELECT 1 FROM `sso_role_permission` existing
    WHERE existing.`role_id`=rp.`role_id`
      AND existing.`permission_id`=new_permission.`id`
      AND existing.`app_id`=1
  );
DELETE FROM `sso_role_permission`
WHERE `app_id`=1
  AND `permission_id` IN (
    SELECT `id` FROM `sso_permission` WHERE `app_id`=1 AND `code`='/index.html'
  );
DELETE FROM `sso_permission` WHERE `app_id`=1 AND `code`='/index.html';

INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 82,NULL,'示例入口','/',3,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=82 AND `code`='/');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 82,NULL,'示例首页','/index.html',2,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=82 AND `code`='/index.html');
INSERT INTO `sso_permission` (`app_id`,`parent_id`,`name`,`code`,`sort`,`icon`,`type`,`is_enable`,`create_time`,`update_time`)
SELECT 82,NULL,'示例用户信息','/userinfo',1,'','function',1,NOW(),NOW() WHERE NOT EXISTS (SELECT 1 FROM `sso_permission` WHERE `app_id`=82 AND `code`='/userinfo');

-- New routes default to the seed system administrator only. Assign them explicitly to
-- custom roles after reviewing least-privilege requirements.
INSERT INTO `sso_role_permission` (`role_id`,`permission_id`,`app_id`)
SELECT 1,p.`id`,p.`app_id` FROM `sso_permission` p
WHERE p.`code` IN ('/','/index.html','/admin/admin/userinfo','/admin/app/credentials','/admin/app/all','/admin/app/validate-code',
                   '/admin/organization/all','/admin/organization/get','/admin/organization/save','/admin/permission/get',
                   '/admin/role-permission/save','/admin/user/get','/admin/user/validate-account','/admin/user/organization/tree',
                   '/admin/user-role.html','/admin/user-role/roles','/admin/user-role/save','/admin/admin.html','/userinfo')
  AND NOT EXISTS (SELECT 1 FROM `sso_role_permission` rp WHERE rp.`role_id`=1 AND rp.`permission_id`=p.`id` AND rp.`app_id`=p.`app_id`);

COMMIT;
SET AUTOCOMMIT TRUE;
