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

 Date: 18/07/2024 13:42:31
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for sys_app
-- ----------------------------
DROP TABLE IF EXISTS `sys_app`;
CREATE TABLE `sys_app` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `code` varchar(50) NOT NULL COMMENT '编码',
  `name` varchar(128) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `client_id` varchar(20) NOT NULL COMMENT '客户端ID',
  `client_secret` varchar(128) NOT NULL COMMENT '客户端密钥',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `unique_code` (`code`),
  UNIQUE KEY `unique_client_id` (`client_id`)
) ENGINE=InnoDB AUTO_INCREMENT=85 DEFAULT CHARSET=utf8 COMMENT='应用表';

-- ----------------------------
-- Records of sys_app
-- ----------------------------
BEGIN;
INSERT INTO `sys_app` VALUES (1, 'smart-sso-server', '单点登录权限管理系统', 20, 1, '1000', 'rokY9BdKh5bHiX/zL26qOg==', '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_app` VALUES (81, 'smart-sso-demo', 'Demo系统', 15, 1, '1001', 'kpA1y7k1uyrcoGhrKvA1Ag==', '2015-11-08 17:16:39', '2024-07-16 11:29:10');
INSERT INTO `sys_app` VALUES (82, 'smart-sso-demo-h5', '前后端分离Demo系统', 10, 1, '1002', '3vjPTgn+9XwV+Q6PRUA5oQ==', '2015-11-08 17:16:39', '2015-11-08 17:16:39');
COMMIT;

-- ----------------------------
-- Table structure for sys_office
-- ----------------------------
DROP TABLE IF EXISTS `sys_office`;
CREATE TABLE `sys_office` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `parent_id` bigint(20) DEFAULT NULL COMMENT '父ID',
  `name` varchar(100) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COMMENT='机构';

-- ----------------------------
-- Records of sys_office
-- ----------------------------
BEGIN;
INSERT INTO `sys_office` VALUES (1, NULL, 'TT公司', 30, 1, '2015-06-02 11:31:44', '2024-07-16 11:28:48');
INSERT INTO `sys_office` VALUES (2, 1, 'XX部门', 30, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_office` VALUES (3, 1, 'YY部门', 20, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
COMMIT;

-- ----------------------------
-- Table structure for sys_permission
-- ----------------------------
DROP TABLE IF EXISTS `sys_permission`;
CREATE TABLE `sys_permission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `app_id` bigint(20) NOT NULL COMMENT '应用ID',
  `parent_id` bigint(20) DEFAULT NULL COMMENT '父ID',
  `name` varchar(50) NOT NULL COMMENT '名称',
  `url` varchar(255) NOT NULL COMMENT '权限URL',
  `sort` int(11) NOT NULL COMMENT '排序',
  `icon` varchar(100) DEFAULT NULL COMMENT '图标',
  `is_menu` int(1) NOT NULL COMMENT '是否菜单',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=66 DEFAULT CHARSET=utf8 COMMENT='权限表';

-- ----------------------------
-- Records of sys_permission
-- ----------------------------
BEGIN;
INSERT INTO `sys_permission` VALUES (2, 1, NULL, '应用', '/admin/app', 59, 'fa fa-th-large', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (3, 1, NULL, '用户', '/admin/user', 79, 'fa-user', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (4, 1, NULL, '角色', '/admin/role', 69, 'fa-users', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (5, 1, NULL, '权限', '/admin/permission', 29, 'fa-key', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (6, 1, 2, '应用新增', '/admin/app/edit', 4, 'fa-plus-circle blue', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (7, 1, 2, '应用启/禁用', '/admin/app/enable', 3, 'fa-lock orange', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (9, 1, 2, '应用删除', '/admin/app/delete', 1, 'fa-trash-o red', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (10, 1, 3, '用户新增', '/admin/user/edit', 6, 'fa-plus-circle blue', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (11, 1, 3, '用户启/禁用', '/admin/user/enable', 5, 'fa-lock orange', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (13, 1, 3, '用户删除', '/admin/user/delete', 3, 'fa-trash-o red', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (14, 1, 3, '重置密码', '/admin/user/resetPassword', 2, 'fa-key grey', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (16, 1, 4, '角色新增', '/admin/role/edit', 5, 'fa-plus-circle blue', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (17, 1, 4, '角色启/禁用', '/admin/role/enable', 4, 'fa-lock orange', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (19, 1, 4, '角色删除', '/admin/role/delete', 2, 'fa-trash-o red', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (20, 1, 4, '角色授权', '/admin/role/allocate', 1, 'fa-cog grey', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (22, 1, 2, '应用列表', '/admin/app/list', 5, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (23, 1, 3, '用户列表', '/admin/user/list', 7, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (24, 1, 4, '角色列表', '/admin/role/list', 6, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (25, 1, 5, '权限树列表', '/admin/permission/nodes', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (26, 1, 2, '应用保存', '/admin/app/save', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (27, 1, 3, '用户保存', '/admin/user/save', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (28, 1, 4, '角色保存', '/admin/role/save', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (29, 1, 5, '权限保存', '/admin/permission/save', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (30, 1, 5, '权限删除', '/admin/permission/delete', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (33, 81, NULL, '菜单1', '/admin/menu1', 100, 'fa-user', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (35, 81, 33, '菜单1新增', '/admin/menu1/edit', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (36, 81, 33, '菜单1删除', '/admin/menu1/delete', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (39, 1, NULL, '导航栏', '/admin/admin/menu', 99, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (41, 1, NULL, '个人中心', '/admin/profile', 89, 'fa fa-desktop', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (42, 1, 41, '修改密码', '/admin/profile/savePassword', 1, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (44, 82, NULL, '栏目管理', '/admin/channel', 100, 'fa fa-th-large', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (47, 82, NULL, '底部菜单管理', '/admin/menu', 110, 'fa fa-list-alt', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (48, 82, NULL, '文章管理', '/admin/article', 90, 'fa fa-file-text', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (49, 82, NULL, '产品管理', '/admin/product', 70, 'fa fa-file-powerpoint-o', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (50, 82, NULL, '产品规格', '/admin/spec', 75, 'fa fa-cubes', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (55, 82, NULL, '首页幻灯片管理', '/admin/slide', 120, 'fa fa-sliders', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (56, 82, NULL, '底部菜单配置', '/admin/channelMenu/edit', 105, 'fa fa-cog', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (59, 81, NULL, '菜单2', '/admin/menu2', 90, '', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (60, 1, NULL, '机构', '/admin/office', 80, 'fa-cogs', 1, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (61, 1, 60, '机构列表', '/admin/office/list', 5, '', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (62, 1, 60, '机构新增', '/admin/app/edit', 4, 'fa-plus-circle blue', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (63, 1, 60, '机构启/禁用', '/admin/office/enable', 3, 'fa-lock orange', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
INSERT INTO `sys_permission` VALUES (65, 1, 60, '机构删除', '/admin/office/delete', 1, 'fa-trash-o red', 0, 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
COMMIT;

-- ----------------------------
-- Table structure for sys_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_role`;
CREATE TABLE `sys_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `description` varchar(200) DEFAULT NULL COMMENT '描述',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8 COMMENT='角色表';

-- ----------------------------
-- Records of sys_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_role` VALUES (1, '系统管理员', 999, '系统管理员', 1, '2015-06-02 11:31:44', '2015-06-02 11:31:44');
COMMIT;

-- ----------------------------
-- Table structure for sys_role_permission
-- ----------------------------
DROP TABLE IF EXISTS `sys_role_permission`;
CREATE TABLE `sys_role_permission` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `role_id` bigint(20) NOT NULL COMMENT '角色ID',
  `permission_id` bigint(20) NOT NULL COMMENT '权限ID',
  `app_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=419 DEFAULT CHARSET=utf8 COMMENT='角色权限表';

-- ----------------------------
-- Records of sys_role_permission
-- ----------------------------
BEGIN;
INSERT INTO `sys_role_permission` VALUES (345, 1, 33, 81);
INSERT INTO `sys_role_permission` VALUES (346, 1, 35, 81);
INSERT INTO `sys_role_permission` VALUES (347, 1, 36, 81);
INSERT INTO `sys_role_permission` VALUES (348, 1, 59, 81);
INSERT INTO `sys_role_permission` VALUES (349, 1, 55, 82);
INSERT INTO `sys_role_permission` VALUES (350, 1, 47, 82);
INSERT INTO `sys_role_permission` VALUES (351, 1, 56, 82);
INSERT INTO `sys_role_permission` VALUES (352, 1, 44, 82);
INSERT INTO `sys_role_permission` VALUES (353, 1, 48, 82);
INSERT INTO `sys_role_permission` VALUES (354, 1, 50, 82);
INSERT INTO `sys_role_permission` VALUES (355, 1, 49, 82);
INSERT INTO `sys_role_permission` VALUES (387, 1, 39, 1);
INSERT INTO `sys_role_permission` VALUES (388, 1, 41, 1);
INSERT INTO `sys_role_permission` VALUES (389, 1, 42, 1);
INSERT INTO `sys_role_permission` VALUES (390, 1, 60, 1);
INSERT INTO `sys_role_permission` VALUES (391, 1, 61, 1);
INSERT INTO `sys_role_permission` VALUES (392, 1, 62, 1);
INSERT INTO `sys_role_permission` VALUES (393, 1, 63, 1);
INSERT INTO `sys_role_permission` VALUES (394, 1, 65, 1);
INSERT INTO `sys_role_permission` VALUES (395, 1, 3, 1);
INSERT INTO `sys_role_permission` VALUES (396, 1, 23, 1);
INSERT INTO `sys_role_permission` VALUES (397, 1, 10, 1);
INSERT INTO `sys_role_permission` VALUES (398, 1, 11, 1);
INSERT INTO `sys_role_permission` VALUES (399, 1, 13, 1);
INSERT INTO `sys_role_permission` VALUES (400, 1, 14, 1);
INSERT INTO `sys_role_permission` VALUES (401, 1, 27, 1);
INSERT INTO `sys_role_permission` VALUES (402, 1, 2, 1);
INSERT INTO `sys_role_permission` VALUES (403, 1, 22, 1);
INSERT INTO `sys_role_permission` VALUES (404, 1, 6, 1);
INSERT INTO `sys_role_permission` VALUES (405, 1, 7, 1);
INSERT INTO `sys_role_permission` VALUES (406, 1, 9, 1);
INSERT INTO `sys_role_permission` VALUES (407, 1, 26, 1);
INSERT INTO `sys_role_permission` VALUES (408, 1, 4, 1);
INSERT INTO `sys_role_permission` VALUES (409, 1, 24, 1);
INSERT INTO `sys_role_permission` VALUES (410, 1, 16, 1);
INSERT INTO `sys_role_permission` VALUES (411, 1, 17, 1);
INSERT INTO `sys_role_permission` VALUES (412, 1, 19, 1);
INSERT INTO `sys_role_permission` VALUES (413, 1, 20, 1);
INSERT INTO `sys_role_permission` VALUES (414, 1, 28, 1);
INSERT INTO `sys_role_permission` VALUES (415, 1, 5, 1);
INSERT INTO `sys_role_permission` VALUES (416, 1, 25, 1);
INSERT INTO `sys_role_permission` VALUES (417, 1, 29, 1);
INSERT INTO `sys_role_permission` VALUES (418, 1, 30, 1);
COMMIT;

-- ----------------------------
-- Table structure for sys_user
-- ----------------------------
DROP TABLE IF EXISTS `sys_user`;
CREATE TABLE `sys_user` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `office_id` bigint(20) NOT NULL COMMENT '机构ID',
  `name` varchar(50) DEFAULT NULL COMMENT '姓名',
  `account` varchar(50) NOT NULL COMMENT '登录名',
  `password` varchar(100) NOT NULL COMMENT '密码(加密)',
  `last_login_time` datetime DEFAULT NULL COMMENT '最后登录时间',
  `login_count` int(11) NOT NULL COMMENT '登录总次数',
  `is_enable` int(1) NOT NULL COMMENT '是否启用',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COMMENT='用户表';

-- ----------------------------
-- Records of sys_user
-- ----------------------------
BEGIN;
INSERT INTO `sys_user` VALUES (2, 3, 'Joe', 'admin', '26524bdf4ea266f131566a89e8f4972c', '2024-07-18 13:34:39', 51, 1, '2015-06-02 11:31:56', '2024-07-18 13:34:39');
COMMIT;

-- ----------------------------
-- Table structure for sys_user_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_user_role`;
CREATE TABLE `sys_user_role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) NOT NULL COMMENT '用户ID ',
  `role_id` bigint(20) NOT NULL COMMENT '角色ID',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=34 DEFAULT CHARSET=utf8 COMMENT='用户角色表';

-- ----------------------------
-- Records of sys_user_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_user_role` VALUES (33, 2, 1);
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
