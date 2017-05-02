/*
Navicat MySQL Data Transfer

Source Server         : Local
Source Server Version : 50620
Source Host           : localhost:3306
Source Database       : sso

Target Server Type    : MYSQL
Target Server Version : 50620
File Encoding         : 65001

Date: 2016-07-11 17:46:04
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for `SYS_APP`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_APP`;
CREATE TABLE `SYS_APP` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `createTime` datetime NOT NULL COMMENT '创建时间',
  `isEnable` bit(1) NOT NULL COMMENT '是否启用',
  `code` varchar(16) NOT NULL COMMENT '编码',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=82 DEFAULT CHARSET=utf8 COMMENT='应用表';

-- ----------------------------
-- Records of SYS_APP
-- ----------------------------
INSERT INTO `SYS_APP` VALUES ('1', '权限管理系统', '999', '2015-06-02 11:31:44', '', 'system_authority');
INSERT INTO `SYS_APP` VALUES ('81', 'Demo管理系统', '9999', '2015-11-08 17:16:39', '', 'system_demo');

-- ----------------------------
-- Table structure for `SYS_PERMISSION`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_PERMISSION`;
CREATE TABLE `SYS_PERMISSION` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `appId` int(11) NOT NULL COMMENT '应用ID',
  `parentId` int(11) DEFAULT NULL COMMENT '父ID',
  `name` varchar(50) NOT NULL COMMENT '名称',
  `url` varchar(255) NOT NULL COMMENT '权限URL',
  `sort` int(11) NOT NULL COMMENT '排序',
  `isMenu` bit(1) NOT NULL COMMENT '是否菜单',
  `isEnable` bit(1) NOT NULL COMMENT '是否启用',
  `icon` varchar(100) DEFAULT NULL COMMENT '图标',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_PERM_REFERENCE_SYS_APP` (`appId`),
  CONSTRAINT `FK_SYS_PERM_REFERENCE_SYS_APP` FOREIGN KEY (`appId`) REFERENCES `SYS_APP` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=43 DEFAULT CHARSET=utf8 COMMENT='权限表';

-- ----------------------------
-- Records of SYS_PERMISSION
-- ----------------------------
INSERT INTO `SYS_PERMISSION` VALUES ('2', '1', null, '应用', '/admin/app', '79', '', '', 'fa-plus-circle blue');
INSERT INTO `SYS_PERMISSION` VALUES ('3', '1', null, '管理员', '/admin/user', '59', '', '', 'fa-user');
INSERT INTO `SYS_PERMISSION` VALUES ('4', '1', null, '角色', '/admin/role', '39', '', '', 'fa-users');
INSERT INTO `SYS_PERMISSION` VALUES ('5', '1', null, '权限', '/admin/permission', '29', '', '', 'fa-key');
INSERT INTO `SYS_PERMISSION` VALUES ('6', '1', '2', '应用新增', '/admin/app/edit', '4', '', '', 'fa-plus-circle blue');
INSERT INTO `SYS_PERMISSION` VALUES ('7', '1', '2', '应用禁用', '/admin/app/enable', '3', '', '', 'fa-lock orange');
INSERT INTO `SYS_PERMISSION` VALUES ('8', '1', '2', '应用启用', '/admin/app/enable', '2', '', '', 'fa-unlock green');
INSERT INTO `SYS_PERMISSION` VALUES ('9', '1', '2', '应用删除', '/admin/app/delete', '1', '', '', 'fa-trash-o red');
INSERT INTO `SYS_PERMISSION` VALUES ('10', '1', '3', '管理员新增', '/admin/user/edit', '6', '', '', 'fa-plus-circle blue');
INSERT INTO `SYS_PERMISSION` VALUES ('11', '1', '3', '管理员禁用', '/admin/user/enable', '5', '', '', 'fa-lock orange');
INSERT INTO `SYS_PERMISSION` VALUES ('12', '1', '3', '管理员启用', '/admin/user/enable', '4', '', '', 'fa-unlock green');
INSERT INTO `SYS_PERMISSION` VALUES ('13', '1', '3', '管理员删除', '/admin/user/delete', '3', '', '', 'fa-trash-o red');
INSERT INTO `SYS_PERMISSION` VALUES ('14', '1', '3', '重置密码', '/admin/user/resetPassword', '2', '', '', 'fa-key grey');
INSERT INTO `SYS_PERMISSION` VALUES ('15', '1', '3', '分配角色', '/admin/user/allocate', '1', '', '', 'fa-cog grey');
INSERT INTO `SYS_PERMISSION` VALUES ('16', '1', '4', '角色新增', '/admin/role/edit', '5', '', '', 'fa-plus-circle blue');
INSERT INTO `SYS_PERMISSION` VALUES ('17', '1', '4', '角色禁用', '/admin/role/enable', '4', '', '', 'fa-lock orange');
INSERT INTO `SYS_PERMISSION` VALUES ('18', '1', '4', '角色启用', '/admin/role/enable', '3', '', '', 'fa-unlock green');
INSERT INTO `SYS_PERMISSION` VALUES ('19', '1', '4', '角色删除', '/admin/role/delete', '2', '', '', 'fa-trash-o red');
INSERT INTO `SYS_PERMISSION` VALUES ('20', '1', '4', '角色授权', '/admin/role/allocate', '1', '', '', 'fa-cog grey');
INSERT INTO `SYS_PERMISSION` VALUES ('22', '1', '2', '应用列表', '/admin/app/list', '5', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('23', '1', '3', '管理员列表', '/admin/user/list', '7', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('24', '1', '4', '角色列表', '/admin/role/list', '6', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('25', '1', '5', '权限树列表', '/admin/permission/nodes', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('26', '1', '2', '应用保存', '/admin/app/save', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('27', '1', '3', '管理员保存', '/admin/user/save', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('28', '1', '4', '角色保存', '/admin/role/save', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('29', '1', '5', '权限保存', '/admin/permission/save', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('30', '1', '5', '权限删除', '/admin/permission/delete', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('33', '81', null, '测试', '/admin/demo', '1', '', '', 'fa-user');
INSERT INTO `SYS_PERMISSION` VALUES ('34', '81', '33', '测试列表', '/admin/demo/list', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('35', '81', '33', '测试新增/修改', '/admin/demo/edit', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('36', '81', '33', '测试删除', '/admin/demo/delete', '1', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('39', '1', null, '导航栏', '/admin/admin/menu', '99', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('40', '81', null, '导航栏', '/admin/admin/menu', '99', '', '', '');
INSERT INTO `SYS_PERMISSION` VALUES ('41', '1', null, '个人中心', '/admin/profile', '89', '', '', 'fa fa-desktop');
INSERT INTO `SYS_PERMISSION` VALUES ('42', '1', '41', '修改密码', '/admin/profile/savePassword', '1', '', '', '');

-- ----------------------------
-- Table structure for `SYS_RE_ROLE_PERMISSION`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_RE_ROLE_PERMISSION`;
CREATE TABLE `SYS_RE_ROLE_PERMISSION` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `roleId` int(11) NOT NULL COMMENT '角色ID',
  `permissionId` int(11) NOT NULL COMMENT '权限ID',
  `appId` int(11) NOT NULL COMMENT '应用ID',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_RE_R_REFERENCE_SYS_PERM` (`permissionId`),
  KEY `FK_SYS_RE_R_REFERENCE_SYS_ROLE` (`roleId`),
  KEY `FK_Reference_9` (`appId`),
  CONSTRAINT `FK_Reference_9` FOREIGN KEY (`appId`) REFERENCES `SYS_APP` (`id`),
  CONSTRAINT `FK_SYS_RE_R_REFERENCE_SYS_PERM` FOREIGN KEY (`permissionId`) REFERENCES `SYS_PERMISSION` (`id`),
  CONSTRAINT `FK_SYS_RE_R_REFERENCE_SYS_ROLE` FOREIGN KEY (`roleId`) REFERENCES `SYS_ROLE` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=243 DEFAULT CHARSET=utf8 COMMENT='角色权限表';

-- ----------------------------
-- Records of SYS_RE_ROLE_PERMISSION
-- ----------------------------
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('176', '4', '40', '81');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('179', '4', '33', '81');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('180', '4', '34', '81');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('181', '4', '35', '81');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('182', '4', '36', '81');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('212', '1', '39', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('213', '1', '41', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('214', '1', '42', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('215', '1', '2', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('216', '1', '22', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('217', '1', '6', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('218', '1', '7', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('219', '1', '8', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('220', '1', '9', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('221', '1', '26', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('222', '1', '3', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('223', '1', '23', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('224', '1', '10', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('225', '1', '11', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('226', '1', '12', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('227', '1', '13', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('228', '1', '14', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('229', '1', '15', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('230', '1', '27', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('231', '1', '4', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('232', '1', '24', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('233', '1', '16', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('234', '1', '17', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('235', '1', '18', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('236', '1', '19', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('237', '1', '20', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('238', '1', '28', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('239', '1', '5', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('240', '1', '25', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('241', '1', '29', '1');
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES ('242', '1', '30', '1');

-- ----------------------------
-- Table structure for `SYS_RE_USER_APP`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_RE_USER_APP`;
CREATE TABLE `SYS_RE_USER_APP` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL COMMENT '用户ID ',
  `appId` int(11) NOT NULL COMMENT '应用ID',
  PRIMARY KEY (`id`),
  KEY `FK_Reference_10` (`appId`),
  KEY `FK_Reference_11` (`userId`),
  CONSTRAINT `FK_Reference_10` FOREIGN KEY (`appId`) REFERENCES `SYS_APP` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `FK_Reference_11` FOREIGN KEY (`userId`) REFERENCES `SYS_USER` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8 COMMENT='用户应用表';

-- ----------------------------
-- Records of SYS_RE_USER_APP
-- ----------------------------
INSERT INTO `SYS_RE_USER_APP` VALUES ('7', '2', '81');
INSERT INTO `SYS_RE_USER_APP` VALUES ('8', '2', '1');

-- ----------------------------
-- Table structure for `SYS_RE_USER_ROLE`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_RE_USER_ROLE`;
CREATE TABLE `SYS_RE_USER_ROLE` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `userId` int(11) NOT NULL COMMENT '用户ID ',
  `roleId` int(11) NOT NULL COMMENT '角色ID',
  `appId` int(11) NOT NULL COMMENT '应用ID',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_RE_U_REFERENCE_SYS_USER` (`userId`),
  KEY `FK_SYS_RE_U_REFERENCE_SYS_ROLE` (`roleId`),
  KEY `FK_Reference_8` (`appId`),
  CONSTRAINT `FK_Reference_8` FOREIGN KEY (`appId`) REFERENCES `SYS_APP` (`id`),
  CONSTRAINT `FK_SYS_RE_U_REFERENCE_SYS_ROLE` FOREIGN KEY (`roleId`) REFERENCES `SYS_ROLE` (`id`),
  CONSTRAINT `FK_SYS_RE_U_REFERENCE_SYS_USER` FOREIGN KEY (`userId`) REFERENCES `SYS_USER` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8 COMMENT='用户角色表';

-- ----------------------------
-- Records of SYS_RE_USER_ROLE
-- ----------------------------
INSERT INTO `SYS_RE_USER_ROLE` VALUES ('7', '2', '4', '81');
INSERT INTO `SYS_RE_USER_ROLE` VALUES ('8', '2', '1', '1');

-- ----------------------------
-- Table structure for `SYS_ROLE`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_ROLE`;
CREATE TABLE `SYS_ROLE` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `appId` int(11) NOT NULL COMMENT '应用ID',
  `name` varchar(50) NOT NULL COMMENT '名称',
  `sort` int(11) NOT NULL COMMENT '排序',
  `description` varchar(200) DEFAULT NULL COMMENT '描述',
  `isEnable` bit(1) NOT NULL COMMENT '是否启用',
  PRIMARY KEY (`id`),
  KEY `FK_SYS_ROLE_REFERENCE_SYS_APP` (`appId`),
  CONSTRAINT `FK_SYS_ROLE_REFERENCE_SYS_APP` FOREIGN KEY (`appId`) REFERENCES `SYS_APP` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COMMENT='角色表';

-- ----------------------------
-- Records of SYS_ROLE
-- ----------------------------
INSERT INTO `SYS_ROLE` VALUES ('1', '1', '单点登录管理员角色', '999', '', '');
INSERT INTO `SYS_ROLE` VALUES ('4', '81', 'demo管理员角色', '1', '', '');

-- ----------------------------
-- Table structure for `SYS_USER`
-- ----------------------------
DROP TABLE IF EXISTS `SYS_USER`;
CREATE TABLE `SYS_USER` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account` varchar(50) NOT NULL COMMENT '登录名',
  `password` varchar(100) NOT NULL COMMENT '密码(加密)',
  `lastLoginIp` varchar(20) DEFAULT NULL COMMENT '最后登录IP',
  `lastLoginTime` datetime DEFAULT NULL COMMENT '最后登录时间',
  `loginCount` int(11) NOT NULL COMMENT '登录总次数',
  `createTime` datetime NOT NULL COMMENT '创建时间',
  `isEnable` bit(1) NOT NULL COMMENT '是否启用',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COMMENT='用户表';

-- ----------------------------
-- Records of SYS_USER
-- ----------------------------
INSERT INTO `SYS_USER` VALUES ('2', 'admin', '26524bdf4ea266f131566a89e8f4972c', null, null, '0', '2015-06-02 11:31:56', '');
