/*
 Navicat MySQL Data Transfer

 Source Server         : local5.7
 Source Server Type    : MySQL
 Source Server Version : 50742
 Source Host           : localhost:3306
 Source Schema         : smart-sso

 Target Server Type    : MySQL
 Target Server Version : 50742
 File Encoding         : 65001

 Date: 30/03/2025 15:55:28
*/


-- ----------------------------
-- Table structure for sso_app
-- ----------------------------
DROP TABLE IF EXISTS `sso_app`;
CREATE TABLE `sso_app` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `code` varchar(50) NOT NULL COMMENT '编码',
  `name` varchar(128) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `client_id` varchar(20) NOT NULL COMMENT '客户端ID',
  `client_secret` varchar(128) NOT NULL COMMENT '客户端密钥',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  CONSTRAINT `unique_code` UNIQUE (`code`),
  CONSTRAINT `unique_client_id` UNIQUE (`client_id`)
) COMMENT='应用表';

-- ----------------------------
-- Records of sso_app
-- ----------------------------
INSERT INTO `sso_app` VALUES (1, 'smart-sso-server', 'Smart-SSO认证授权中心', 20, 1, '1000', 'rokY9BdKh5bHiX/zL26qOg==', '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_app` VALUES (82, 'smart-sso-demo', 'Smart-SSO客户端示例', 10, 1, '1002', '3vjPTgn+9XwV+Q6PRUA5oQ==', '2015-11-08 17:16:39', '2015-11-08 17:16:39');

-- ----------------------------
-- Table structure for sso_organization
-- ----------------------------
DROP TABLE IF EXISTS `sso_organization`;
CREATE TABLE `sso_organization` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `parent_id` bigint(20) DEFAULT NULL COMMENT '父ID',
  `name` varchar(100) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='机构';

-- ----------------------------
-- Records of sso_organization
-- ----------------------------
INSERT INTO `sso_organization` VALUES (1, NULL, 'TT公司', 30, 1, '2015-06-02 11:31:44', '2024-07-16 11:28:48');
INSERT INTO `sso_organization` VALUES (2, 1, 'XX部门', 30, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_organization` VALUES (3, 1, 'YY部门', 20, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');

-- ----------------------------
-- Table structure for sso_permission
-- ----------------------------
DROP TABLE IF EXISTS `sso_permission`;
CREATE TABLE `sso_permission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `app_id` bigint(20) NOT NULL COMMENT '应用ID',
  `parent_id` bigint(20) DEFAULT NULL COMMENT '父ID',
  `name` varchar(50) NOT NULL COMMENT '名称',
  `code` varchar(255) NOT NULL COMMENT '稳定权限码',
  `url` varchar(255) DEFAULT NULL COMMENT '菜单地址，仅菜单类型使用',
  `sort` int(11) NOT NULL COMMENT '排序',
  `icon` varchar(100) DEFAULT NULL COMMENT '图标',
  `type` varchar(20) NOT NULL COMMENT '权限类型(menu=菜单, function=功能)',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  CONSTRAINT `unique_app_permission_code` UNIQUE (`app_id`, `code`)
) COMMENT='权限表';

-- ----------------------------
-- Records of sso_permission
-- ----------------------------
INSERT INTO `sso_permission` VALUES (2, 1, NULL, '应用管理', 'menu:app', '/admin/app.html', 59, 'fa fa-th-large', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (3, 1, NULL, '用户管理', 'menu:user', '/admin/user.html', 79, 'fa-user', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (4, 1, NULL, '角色管理', 'menu:role', '/admin/role.html', 69, 'fa-briefcase', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (5, 1, NULL, '权限管理', 'menu:permission', '/admin/permission.html', 29, 'fa-key', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (7, 1, 2, '应用启/禁用', 'app:status-write', NULL, 3, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (9, 1, 2, '应用删除', 'app:delete', NULL, 1, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (11, 1, 3, '用户启/禁用', 'user:status-write', NULL, 5, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (13, 1, 3, '用户删除', 'user:delete', NULL, 3, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (14, 1, 3, '重置密码', 'user:password-reset', NULL, 2, 'fa-key grey', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (17, 1, 4, '角色启/禁用', 'role:status-write', NULL, 4, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (19, 1, 4, '角色删除', 'role:delete', NULL, 2, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (22, 1, 2, '应用查看', 'app:read', NULL, 5, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (23, 1, 3, '用户查看', 'user:read', NULL, 7, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (24, 1, 4, '角色查看', 'role:read', NULL, 6, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (25, 1, 5, '权限查看', 'permission:read', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (26, 1, 2, '应用编辑', 'app:write', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (27, 1, 3, '用户编辑', 'user:write', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (28, 1, 4, '角色编辑', 'role:write', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (29, 1, 5, '权限编辑', 'permission:write', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (30, 1, 5, '权限删除', 'permission:delete', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (41, 1, NULL, '个人中心', 'menu:profile', '/admin/profile.html', 89, 'fa fa-desktop', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (42, 1, 41, '修改密码', 'profile:password-write', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (44, 82, NULL, '首页', 'menu:demo-home', '/index.html', 100, 'fa fa-home', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (47, 82, 44, '用户信息', 'demo:userinfo', NULL, 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (60, 1, NULL, '机构管理', 'menu:organization', '/admin/organization.html', 80, 'fa-cogs', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (61, 1, 60, '机构查看', 'organization:read', NULL, 5, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (63, 1, 60, '机构启/禁用', 'organization:status-write', NULL, 3, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (65, 1, 60, '机构删除', 'organization:delete', NULL, 1, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (66, 1, NULL, '会话管理', 'menu:session', '/admin/login-user.html', 1, 'fa-users', 'menu', 1, '2025-03-28 15:05:31', '2025-03-28 15:05:31');
INSERT INTO `sso_permission` VALUES (67, 1, 66, '会话查看', 'session:read', NULL, 5, '', 'function', 1, '2025-03-31 15:52:25', '2025-03-31 15:52:25');
INSERT INTO `sso_permission` VALUES (68, 1, 66, '强制下线', 'session:terminate', NULL, 2, '', 'function', 1, '2025-03-31 15:52:55', '2025-03-31 15:53:33');
INSERT INTO `sso_permission` VALUES (77, 1, 60, '机构编辑', 'organization:write', NULL, 2, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (79, 1, 4, '角色授权', 'role:permission-write', NULL, 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (85, 1, 3, '分配用户角色', 'user:role-write', NULL, 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');

-- ----------------------------
-- Table structure for sso_role
-- ----------------------------
DROP TABLE IF EXISTS `sso_role`;
CREATE TABLE `sso_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `description` varchar(200) DEFAULT NULL COMMENT '描述',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='角色表';

-- ----------------------------
-- Records of sso_role
-- ----------------------------
INSERT INTO `sso_role` VALUES (1, '系统管理员', 999, '系统管理员', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');

-- ----------------------------
-- Table structure for sso_role_permission
-- ----------------------------
DROP TABLE IF EXISTS `sso_role_permission`;
CREATE TABLE `sso_role_permission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `role_id` bigint(20) NOT NULL COMMENT '角色ID',
  `permission_id` bigint(20) NOT NULL COMMENT '权限ID',
  `app_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`)
) COMMENT='角色权限表';

-- ----------------------------
-- Records of sso_role_permission
-- ----------------------------
INSERT INTO `sso_role_permission` VALUES (350, 1, 47, 82);
INSERT INTO `sso_role_permission` VALUES (352, 1, 44, 82);
INSERT INTO `sso_role_permission` VALUES (420, 1, 41, 1);
INSERT INTO `sso_role_permission` VALUES (421, 1, 42, 1);
INSERT INTO `sso_role_permission` VALUES (422, 1, 60, 1);
INSERT INTO `sso_role_permission` VALUES (423, 1, 61, 1);
INSERT INTO `sso_role_permission` VALUES (425, 1, 63, 1);
INSERT INTO `sso_role_permission` VALUES (426, 1, 65, 1);
INSERT INTO `sso_role_permission` VALUES (427, 1, 3, 1);
INSERT INTO `sso_role_permission` VALUES (428, 1, 23, 1);
INSERT INTO `sso_role_permission` VALUES (430, 1, 11, 1);
INSERT INTO `sso_role_permission` VALUES (431, 1, 13, 1);
INSERT INTO `sso_role_permission` VALUES (432, 1, 14, 1);
INSERT INTO `sso_role_permission` VALUES (433, 1, 27, 1);
INSERT INTO `sso_role_permission` VALUES (434, 1, 4, 1);
INSERT INTO `sso_role_permission` VALUES (435, 1, 24, 1);
INSERT INTO `sso_role_permission` VALUES (437, 1, 17, 1);
INSERT INTO `sso_role_permission` VALUES (438, 1, 19, 1);
INSERT INTO `sso_role_permission` VALUES (440, 1, 28, 1);
INSERT INTO `sso_role_permission` VALUES (441, 1, 2, 1);
INSERT INTO `sso_role_permission` VALUES (442, 1, 22, 1);
INSERT INTO `sso_role_permission` VALUES (444, 1, 7, 1);
INSERT INTO `sso_role_permission` VALUES (445, 1, 9, 1);
INSERT INTO `sso_role_permission` VALUES (446, 1, 26, 1);
INSERT INTO `sso_role_permission` VALUES (447, 1, 5, 1);
INSERT INTO `sso_role_permission` VALUES (448, 1, 25, 1);
INSERT INTO `sso_role_permission` VALUES (449, 1, 29, 1);
INSERT INTO `sso_role_permission` VALUES (450, 1, 30, 1);
INSERT INTO `sso_role_permission` VALUES (451, 1, 66, 1);
INSERT INTO `sso_role_permission` VALUES (452, 1, 67, 1);
INSERT INTO `sso_role_permission` VALUES (453, 1, 68, 1);
INSERT INTO `sso_role_permission` VALUES (462, 1, 77, 1);
INSERT INTO `sso_role_permission` VALUES (464, 1, 79, 1);
INSERT INTO `sso_role_permission` VALUES (470, 1, 85, 1);

-- ----------------------------
-- Table structure for sso_user
-- ----------------------------
DROP TABLE IF EXISTS `sso_user`;
CREATE TABLE `sso_user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `organization_id` bigint(20) NOT NULL COMMENT '机构ID',
  `name` varchar(50) DEFAULT NULL COMMENT '姓名',
  `account` varchar(50) NOT NULL COMMENT '登录名',
  `password` varchar(100) NOT NULL COMMENT '密码(加密)',
  `last_login_time` datetime DEFAULT NULL COMMENT '最后登录时间',
  `login_count` int(11) NOT NULL COMMENT '登录总次数',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='用户表';

-- ----------------------------
-- Records of sso_user
-- ----------------------------
INSERT INTO `sso_user` VALUES (2, 3, 'Joe', 'admin', '26524bdf4ea266f131566a89e8f4972c', '2025-03-31 15:54:46', 54, 1, '2015-06-02 11:31:56', '2025-03-31 15:54:46');

-- ----------------------------
-- Table structure for sso_user_role
-- ----------------------------
DROP TABLE IF EXISTS `sso_user_role`;
CREATE TABLE `sso_user_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) NOT NULL COMMENT '用户ID ',
  `role_id` bigint(20) NOT NULL COMMENT '角色ID',
  PRIMARY KEY (`id`)
) COMMENT='用户角色表';

-- ----------------------------
-- Records of sso_user_role
-- ----------------------------
INSERT INTO `sso_user_role` VALUES (33, 2, 1);
