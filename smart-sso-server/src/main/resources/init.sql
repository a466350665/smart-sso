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
  `code` varchar(255) NOT NULL COMMENT '权限编码',
  `sort` int(11) NOT NULL COMMENT '排序',
  `icon` varchar(100) DEFAULT NULL COMMENT '图标',
  `type` varchar(20) NOT NULL COMMENT '权限类型(menu=菜单, function=功能)',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) COMMENT='权限表';

-- ----------------------------
-- Records of sso_permission
-- ----------------------------
INSERT INTO `sso_permission` VALUES (2, 1, NULL, '应用管理', '/admin/app.html', 59, 'fa fa-th-large', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (3, 1, NULL, '用户管理', '/admin/user.html', 79, 'fa-user', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (4, 1, NULL, '角色管理', '/admin/role.html', 69, 'fa-briefcase', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (5, 1, NULL, '权限管理', '/admin/permission.html', 29, 'fa-key', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (7, 1, 2, '应用启/禁用', '/admin/app/enable', 3, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (9, 1, 2, '应用删除', '/admin/app/delete', 1, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (11, 1, 3, '用户启/禁用', '/admin/user/enable', 5, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (13, 1, 3, '用户删除', '/admin/user/delete', 3, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (14, 1, 3, '重置密码', '/admin/user/reset-password', 2, 'fa-key grey', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (17, 1, 4, '角色启/禁用', '/admin/role/enable', 4, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (19, 1, 4, '角色删除', '/admin/role/delete', 2, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (20, 1, 4, '角色授权页', '/admin/role-permission.html', 1, 'fa-cog grey', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (22, 1, 2, '应用列表', '/admin/app/list', 5, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (23, 1, 3, '用户列表', '/admin/user/list', 7, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (24, 1, 4, '角色列表', '/admin/role/list', 6, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (25, 1, 5, '权限树列表', '/admin/permission/tree', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (26, 1, 2, '应用编辑', '/admin/app/save', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (27, 1, 3, '用户编辑', '/admin/user/save', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (28, 1, 4, '角色编辑', '/admin/role/save', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (29, 1, 5, '权限编辑', '/admin/permission/save', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (30, 1, 5, '权限删除', '/admin/permission/delete', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (39, 1, NULL, '导航栏', '/admin/admin/menu', 99, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (41, 1, NULL, '个人中心', '/admin/profile.html', 89, 'fa fa-desktop', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (42, 1, 41, '修改密码', '/admin/profile/save-password', 1, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (44, 82, NULL, '栏目管理', '/channel.html', 100, 'fa fa-th-large', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (47, 82, NULL, '底部菜单管理', '/menu.html', 110, 'fa fa-list-alt', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (48, 82, NULL, '文章管理', '/article.html', 90, 'fa fa-file-text', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (49, 82, NULL, '产品管理', '/product.html', 70, 'fa fa-file-powerpoint-o', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (50, 82, NULL, '产品规格', '/spec.html', 75, 'fa fa-cubes', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (55, 82, NULL, '首页幻灯片管理', '/slide.html', 120, 'fa fa-sliders', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (56, 82, NULL, '底部菜单配置', '/channelMenu/edit.html', 105, 'fa fa-cog', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (60, 1, NULL, '机构管理', '/admin/organization.html', 80, 'fa-cogs', 'menu', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (61, 1, 60, '机构列表', '/admin/organization/list', 5, '', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (63, 1, 60, '机构启/禁用', '/admin/organization/enable', 3, 'fa-lock orange', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (65, 1, 60, '机构删除', '/admin/organization/delete', 1, 'fa-trash-o red', 'function', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sso_permission` VALUES (66, 1, NULL, '会话管理', '/admin/login-user.html', 1, 'fa-users', 'menu', 1, '2025-03-28 15:05:31', '2025-03-28 15:05:31');
INSERT INTO `sso_permission` VALUES (67, 1, 66, '会话列表', '/admin/login-user/list', 5, '', 'function', 1, '2025-03-31 15:52:25', '2025-03-31 15:52:25');
INSERT INTO `sso_permission` VALUES (68, 1, 66, '强制下线', '/admin/login-user/logout', 2, '', 'function', 1, '2025-03-31 15:52:55', '2025-03-31 15:53:33');
INSERT INTO `sso_permission` VALUES (69, 1, NULL, '管理台入口', '/', 100, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (70, 1, NULL, '管理台首页', '/index.html', 99, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (71, 1, 41, '当前用户信息', '/admin/admin/userinfo', 98, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (72, 1, 2, '查看应用密钥', '/admin/app/credentials', 2, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (73, 1, 2, '全部启用应用', '/admin/app/all', 4, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (74, 1, 2, '验证应用编码', '/admin/app/validate-code', 3, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (75, 1, 60, '全部启用机构', '/admin/organization/all', 4, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (76, 1, 60, '机构信息', '/admin/organization/get', 3, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (77, 1, 60, '机构编辑', '/admin/organization/save', 2, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (78, 1, 5, '权限信息', '/admin/permission/get', 3, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (79, 1, 4, '角色授权提交', '/admin/role-permission/save', 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (80, 1, 3, '用户信息', '/admin/user/get', 5, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (81, 1, 3, '验证登录名', '/admin/user/validate-account', 4, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (82, 1, 3, '用户机构树', '/admin/user/organization/tree', 3, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (83, 1, 3, '分配用户角色页', '/admin/user-role.html', 2, 'fa-cog grey', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (84, 1, 3, '用户角色列表', '/admin/user-role/roles', 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (85, 1, 3, '分配用户角色', '/admin/user-role/save', 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (86, 1, NULL, '管理台内容页', '/admin/admin.html', 99, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (87, 82, NULL, '示例入口', '/', 3, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (88, 82, NULL, '示例首页', '/index.html', 2, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');
INSERT INTO `sso_permission` VALUES (89, 82, NULL, '示例用户信息', '/userinfo', 1, '', 'function', 1, '2026-10-28 00:00:00', '2026-10-28 00:00:00');

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
INSERT INTO `sso_role_permission` VALUES (349, 1, 55, 82);
INSERT INTO `sso_role_permission` VALUES (350, 1, 47, 82);
INSERT INTO `sso_role_permission` VALUES (351, 1, 56, 82);
INSERT INTO `sso_role_permission` VALUES (352, 1, 44, 82);
INSERT INTO `sso_role_permission` VALUES (353, 1, 48, 82);
INSERT INTO `sso_role_permission` VALUES (354, 1, 50, 82);
INSERT INTO `sso_role_permission` VALUES (355, 1, 49, 82);
INSERT INTO `sso_role_permission` VALUES (419, 1, 39, 1);
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
INSERT INTO `sso_role_permission` VALUES (439, 1, 20, 1);
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
INSERT INTO `sso_role_permission` VALUES (454, 1, 69, 1);
INSERT INTO `sso_role_permission` VALUES (455, 1, 70, 1);
INSERT INTO `sso_role_permission` VALUES (456, 1, 71, 1);
INSERT INTO `sso_role_permission` VALUES (457, 1, 72, 1);
INSERT INTO `sso_role_permission` VALUES (458, 1, 73, 1);
INSERT INTO `sso_role_permission` VALUES (459, 1, 74, 1);
INSERT INTO `sso_role_permission` VALUES (460, 1, 75, 1);
INSERT INTO `sso_role_permission` VALUES (461, 1, 76, 1);
INSERT INTO `sso_role_permission` VALUES (462, 1, 77, 1);
INSERT INTO `sso_role_permission` VALUES (463, 1, 78, 1);
INSERT INTO `sso_role_permission` VALUES (464, 1, 79, 1);
INSERT INTO `sso_role_permission` VALUES (465, 1, 80, 1);
INSERT INTO `sso_role_permission` VALUES (466, 1, 81, 1);
INSERT INTO `sso_role_permission` VALUES (467, 1, 82, 1);
INSERT INTO `sso_role_permission` VALUES (468, 1, 83, 1);
INSERT INTO `sso_role_permission` VALUES (469, 1, 84, 1);
INSERT INTO `sso_role_permission` VALUES (470, 1, 85, 1);
INSERT INTO `sso_role_permission` VALUES (471, 1, 86, 1);
INSERT INTO `sso_role_permission` VALUES (472, 1, 87, 82);
INSERT INTO `sso_role_permission` VALUES (473, 1, 88, 82);
INSERT INTO `sso_role_permission` VALUES (474, 1, 89, 82);

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
