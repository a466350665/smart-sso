/*
Navicat MySQL Data Transfer

Source Server         : localhost
Source Server Version : 50637
Source Host           : localhost:3306
Source Database       : smart-sso

Target Server Type    : MYSQL
Target Server Version : 50637
File Encoding         : 65001

Date: 2018-09-13 16:29:46
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for `sys_app`
-- ----------------------------
DROP TABLE IF EXISTS `sys_app`;
CREATE TABLE `sys_app` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `createTime` datetime NOT NULL COMMENT '创建时间',
  `isEnable` int(1) NOT NULL COMMENT '是否启用',
  `code` varchar(16) NOT NULL COMMENT '编码',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=84 DEFAULT CHARSET=utf8 COMMENT='应用表';

-- ----------------------------
-- Records of sys_app
-- ----------------------------
INSERT INTO `sys_app` VALUES ('1', '单点登录权限管理系统', '20', '2015-06-02 11:31:44', '1', 'smart-sso-server');
INSERT INTO `sys_app` VALUES ('81', 'Demo管理系统', '15', '2015-11-08 17:16:39', '1', 'smart-sso-demo');
INSERT INTO `sys_app` VALUES ('82', '内容管理系统', '10', '2015-11-08 17:16:39', '1', 'smart-cms');

-- ----------------------------
-- Table structure for `sys_permission`
-- ----------------------------
DROP TABLE IF EXISTS `sys_permission`;
CREATE TABLE `sys_permission` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `appId` int(11) NOT NULL COMMENT '应用ID',
  `parentId` int(11) DEFAULT NULL COMMENT '父ID',
  `name` varchar(50) NOT NULL COMMENT '名称',
  `url` varchar(255) NOT NULL COMMENT '权限URL',
  `sort` int(11) NOT NULL COMMENT '排序',
  `isMenu` int(1) NOT NULL COMMENT '是否菜单',
  `isEnable` int(1) NOT NULL COMMENT '是否启用',
  `icon` varchar(100) DEFAULT NULL COMMENT '图标',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_PERM_REFERENCE_sys_app` (`appId`),
  CONSTRAINT `FK_SYS_PERM_REFERENCE_sys_app` FOREIGN KEY (`appId`) REFERENCES `sys_app` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=60 DEFAULT CHARSET=utf8 COMMENT='权限表';

-- ----------------------------
-- Records of sys_permission
-- ----------------------------
INSERT INTO `sys_permission` VALUES ('2', '1', null, '应用', '/admin/app', '69', '1', '1', 'fa-plus-circle blue');
INSERT INTO `sys_permission` VALUES ('3', '1', null, '用户', '/admin/user', '79', '1', '1', 'fa-user');
INSERT INTO `sys_permission` VALUES ('4', '1', null, '角色', '/admin/role', '59', '1', '1', 'fa-users');
INSERT INTO `sys_permission` VALUES ('5', '1', null, '权限', '/admin/permission', '29', '1', '1', 'fa-key');
INSERT INTO `sys_permission` VALUES ('6', '1', '2', '应用新增', '/admin/app/edit', '4', '0', '1', 'fa-plus-circle blue');
INSERT INTO `sys_permission` VALUES ('7', '1', '2', '应用禁用', '/admin/app/enable', '3', '0', '1', 'fa-lock orange');
INSERT INTO `sys_permission` VALUES ('8', '1', '2', '应用启用', '/admin/app/enable', '2', '0', '1', 'fa-unlock green');
INSERT INTO `sys_permission` VALUES ('9', '1', '2', '应用删除', '/admin/app/delete', '1', '0', '1', 'fa-trash-o red');
INSERT INTO `sys_permission` VALUES ('10', '1', '3', '用户新增', '/admin/user/edit', '6', '0', '1', 'fa-plus-circle blue');
INSERT INTO `sys_permission` VALUES ('11', '1', '3', '用户禁用', '/admin/user/enable', '5', '0', '1', 'fa-lock orange');
INSERT INTO `sys_permission` VALUES ('12', '1', '3', '用户启用', '/admin/user/enable', '4', '0', '1', 'fa-unlock green');
INSERT INTO `sys_permission` VALUES ('13', '1', '3', '用户删除', '/admin/user/delete', '3', '0', '1', 'fa-trash-o red');
INSERT INTO `sys_permission` VALUES ('14', '1', '3', '重置密码', '/admin/user/resetPassword', '2', '0', '1', 'fa-key grey');
INSERT INTO `sys_permission` VALUES ('16', '1', '4', '角色新增', '/admin/role/edit', '5', '0', '1', 'fa-plus-circle blue');
INSERT INTO `sys_permission` VALUES ('17', '1', '4', '角色禁用', '/admin/role/enable', '4', '0', '1', 'fa-lock orange');
INSERT INTO `sys_permission` VALUES ('18', '1', '4', '角色启用', '/admin/role/enable', '3', '0', '1', 'fa-unlock green');
INSERT INTO `sys_permission` VALUES ('19', '1', '4', '角色删除', '/admin/role/delete', '2', '0', '1', 'fa-trash-o red');
INSERT INTO `sys_permission` VALUES ('20', '1', '4', '角色授权', '/admin/role/allocate', '1', '0', '1', 'fa-cog grey');
INSERT INTO `sys_permission` VALUES ('22', '1', '2', '应用列表', '/admin/app/list', '5', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('23', '1', '3', '用户列表', '/admin/user/list', '7', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('24', '1', '4', '角色列表', '/admin/role/list', '6', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('25', '1', '5', '权限树列表', '/admin/permission/nodes', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('26', '1', '2', '应用保存', '/admin/app/save', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('27', '1', '3', '用户保存', '/admin/user/save', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('28', '1', '4', '角色保存', '/admin/role/save', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('29', '1', '5', '权限保存', '/admin/permission/save', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('30', '1', '5', '权限删除', '/admin/permission/delete', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('33', '81', null, '菜单1', '/admin/men1', '100', '1', '1', 'fa-user');
INSERT INTO `sys_permission` VALUES ('35', '81', '33', '菜单1新增', '/admin/menu1/edit', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('36', '81', '33', '菜单1删除', '/admin/menu1/delete', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('39', '1', null, '导航栏', '/admin/admin/menu', '99', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('41', '1', null, '个人中心', '/admin/profile', '89', '1', '1', 'fa fa-desktop');
INSERT INTO `sys_permission` VALUES ('42', '1', '41', '修改密码', '/admin/profile/savePassword', '1', '0', '1', '');
INSERT INTO `sys_permission` VALUES ('44', '82', null, '栏目管理', '/admin/channel', '100', '1', '1', 'fa fa-th-large');
INSERT INTO `sys_permission` VALUES ('47', '82', null, '底部菜单管理', '/admin/menu', '110', '1', '1', 'fa fa-list-alt');
INSERT INTO `sys_permission` VALUES ('48', '82', null, '文章管理', '/admin/article', '90', '1', '1', 'fa fa-file-text');
INSERT INTO `sys_permission` VALUES ('49', '82', null, '产品管理', '/admin/product', '70', '1', '1', 'fa fa-file-powerpoint-o');
INSERT INTO `sys_permission` VALUES ('50', '82', null, '产品规格', '/admin/spec', '75', '1', '1', 'fa fa-cubes');
INSERT INTO `sys_permission` VALUES ('55', '82', null, '首页幻灯片管理', '/admin/slide', '120', '1', '1', 'fa fa-sliders');
INSERT INTO `sys_permission` VALUES ('56', '82', null, '底部菜单配置', '/admin/channelMenu/edit', '105', '1', '1', 'fa fa-cog');
INSERT INTO `sys_permission` VALUES ('59', '81', null, '菜单2', '/admin/menu2', '90', '1', '1', '');

-- ----------------------------
-- Table structure for `sys_re_role_permission`
-- ----------------------------
DROP TABLE IF EXISTS `sys_re_role_permission`;
CREATE TABLE `sys_re_role_permission` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `roleId` int(11) NOT NULL COMMENT '角色ID',
  `permissionId` int(11) NOT NULL COMMENT '权限ID',
  `appId` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_SYS_RE_R_REFERENCE_SYS_PERM` (`permissionId`),
  KEY `FK_SYS_RE_R_REFERENCE_sys_role` (`roleId`),
  CONSTRAINT `FK_SYS_RE_R_REFERENCE_SYS_PERM` FOREIGN KEY (`permissionId`) REFERENCES `sys_permission` (`id`),
  CONSTRAINT `FK_SYS_RE_R_REFERENCE_sys_role` FOREIGN KEY (`roleId`) REFERENCES `sys_role` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=356 DEFAULT CHARSET=utf8 COMMENT='角色权限表';

-- ----------------------------
-- Records of sys_re_role_permission
-- ----------------------------
INSERT INTO `sys_re_role_permission` VALUES ('291', '1', '39', '1');
INSERT INTO `sys_re_role_permission` VALUES ('292', '1', '41', '1');
INSERT INTO `sys_re_role_permission` VALUES ('293', '1', '42', '1');
INSERT INTO `sys_re_role_permission` VALUES ('294', '1', '3', '1');
INSERT INTO `sys_re_role_permission` VALUES ('295', '1', '23', '1');
INSERT INTO `sys_re_role_permission` VALUES ('296', '1', '10', '1');
INSERT INTO `sys_re_role_permission` VALUES ('297', '1', '11', '1');
INSERT INTO `sys_re_role_permission` VALUES ('298', '1', '12', '1');
INSERT INTO `sys_re_role_permission` VALUES ('299', '1', '13', '1');
INSERT INTO `sys_re_role_permission` VALUES ('300', '1', '14', '1');
INSERT INTO `sys_re_role_permission` VALUES ('302', '1', '27', '1');
INSERT INTO `sys_re_role_permission` VALUES ('303', '1', '2', '1');
INSERT INTO `sys_re_role_permission` VALUES ('304', '1', '22', '1');
INSERT INTO `sys_re_role_permission` VALUES ('305', '1', '6', '1');
INSERT INTO `sys_re_role_permission` VALUES ('306', '1', '7', '1');
INSERT INTO `sys_re_role_permission` VALUES ('307', '1', '8', '1');
INSERT INTO `sys_re_role_permission` VALUES ('308', '1', '9', '1');
INSERT INTO `sys_re_role_permission` VALUES ('309', '1', '26', '1');
INSERT INTO `sys_re_role_permission` VALUES ('310', '1', '4', '1');
INSERT INTO `sys_re_role_permission` VALUES ('311', '1', '24', '1');
INSERT INTO `sys_re_role_permission` VALUES ('312', '1', '16', '1');
INSERT INTO `sys_re_role_permission` VALUES ('313', '1', '17', '1');
INSERT INTO `sys_re_role_permission` VALUES ('314', '1', '18', '1');
INSERT INTO `sys_re_role_permission` VALUES ('315', '1', '19', '1');
INSERT INTO `sys_re_role_permission` VALUES ('316', '1', '20', '1');
INSERT INTO `sys_re_role_permission` VALUES ('317', '1', '28', '1');
INSERT INTO `sys_re_role_permission` VALUES ('318', '1', '5', '1');
INSERT INTO `sys_re_role_permission` VALUES ('319', '1', '25', '1');
INSERT INTO `sys_re_role_permission` VALUES ('320', '1', '29', '1');
INSERT INTO `sys_re_role_permission` VALUES ('321', '1', '30', '1');
INSERT INTO `sys_re_role_permission` VALUES ('345', '1', '33', '81');
INSERT INTO `sys_re_role_permission` VALUES ('346', '1', '35', '81');
INSERT INTO `sys_re_role_permission` VALUES ('347', '1', '36', '81');
INSERT INTO `sys_re_role_permission` VALUES ('348', '1', '59', '81');
INSERT INTO `sys_re_role_permission` VALUES ('349', '1', '55', '82');
INSERT INTO `sys_re_role_permission` VALUES ('350', '1', '47', '82');
INSERT INTO `sys_re_role_permission` VALUES ('351', '1', '56', '82');
INSERT INTO `sys_re_role_permission` VALUES ('352', '1', '44', '82');
INSERT INTO `sys_re_role_permission` VALUES ('353', '1', '48', '82');
INSERT INTO `sys_re_role_permission` VALUES ('354', '1', '50', '82');
INSERT INTO `sys_re_role_permission` VALUES ('355', '1', '49', '82');

-- ----------------------------
-- Table structure for `sys_re_user_role`
-- ----------------------------
DROP TABLE IF EXISTS `sys_re_user_role`;
CREATE TABLE `sys_re_user_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL COMMENT '用户ID ',
  `roleId` int(11) NOT NULL COMMENT '角色ID',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_RE_U_REFERENCE_sys_user` (`userId`),
  KEY `FK_SYS_RE_U_REFERENCE_sys_role` (`roleId`),
  CONSTRAINT `FK_SYS_RE_U_REFERENCE_sys_role` FOREIGN KEY (`roleId`) REFERENCES `sys_role` (`id`),
  CONSTRAINT `FK_SYS_RE_U_REFERENCE_sys_user` FOREIGN KEY (`userId`) REFERENCES `sys_user` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=20 DEFAULT CHARSET=utf8 COMMENT='用户角色表';

-- ----------------------------
-- Records of sys_re_user_role
-- ----------------------------
INSERT INTO `sys_re_user_role` VALUES ('16', '2', '1');

-- ----------------------------
-- Table structure for `sys_role`
-- ----------------------------
DROP TABLE IF EXISTS `sys_role`;
CREATE TABLE `sys_role` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `description` varchar(200) DEFAULT NULL COMMENT '描述',
  `isEnable` int(1) NOT NULL COMMENT '是否启用',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=utf8 COMMENT='角色表';

-- ----------------------------
-- Records of sys_role
-- ----------------------------
INSERT INTO `sys_role` VALUES ('1', '系统管理员', '999', '系统管理员', '1');

-- ----------------------------
-- Table structure for `sys_user`
-- ----------------------------
DROP TABLE IF EXISTS `sys_user`;
CREATE TABLE `sys_user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account` varchar(50) NOT NULL COMMENT '登录名',
  `password` varchar(100) NOT NULL COMMENT '密码(加密)',
  `lastLoginIp` varchar(20) DEFAULT NULL COMMENT '最后登录IP',
  `lastLoginTime` datetime DEFAULT NULL COMMENT '最后登录时间',
  `loginCount` int(11) NOT NULL COMMENT '登录总次数',
  `createTime` datetime NOT NULL COMMENT '创建时间',
  `isEnable` int(1) NOT NULL COMMENT '是否启用',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COMMENT='用户表';

-- ----------------------------
-- Records of sys_user
-- ----------------------------
INSERT INTO `sys_user` VALUES ('2', 'admin', '26524bdf4ea266f131566a89e8f4972c', '0:0:0:0:0:0:0:1', '2018-09-13 16:28:02', '216', '2015-06-02 11:31:56', '1');
