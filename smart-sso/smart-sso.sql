-- MySQL dump 10.13  Distrib 5.7.18, for osx10.12 (x86_64)
--
-- Host: localhost    Database: smarty-sso
-- ------------------------------------------------------
-- Server version	5.7.18

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `SYS_APP`
--

DROP TABLE IF EXISTS `SYS_APP`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `SYS_APP` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL COMMENT '名称',
  `code` varchar(16) NOT NULL COMMENT '编码',
  `sort` int(11) NOT NULL COMMENT '排序',
  `createTime` datetime NOT NULL COMMENT '创建时间',
  `isEnable` bit(1) NOT NULL COMMENT '是否启用',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=83 DEFAULT CHARSET=utf8 COMMENT='应用表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_APP`
--

LOCK TABLES `SYS_APP` WRITE;
/*!40000 ALTER TABLE `SYS_APP` DISABLE KEYS */;
INSERT INTO `SYS_APP` VALUES (1,'权限管理系统','system_authority',999,'2015-06-02 11:31:44',''),(81,'Demo管理系统','system_demo',9999,'2015-11-08 17:16:39',''),(82,'财务报销系统','finance',3,'2017-05-28 22:36:57','');
/*!40000 ALTER TABLE `SYS_APP` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_PERMISSION`
--

DROP TABLE IF EXISTS `SYS_PERMISSION`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_PERMISSION`
--

LOCK TABLES `SYS_PERMISSION` WRITE;
/*!40000 ALTER TABLE `SYS_PERMISSION` DISABLE KEYS */;
INSERT INTO `SYS_PERMISSION` VALUES (2,1,NULL,'应用','/admin/app',79,'','','fa-plus-circle blue'),(3,1,NULL,'管理员','/admin/user',59,'','','fa-user'),(4,1,NULL,'角色','/admin/role',39,'','','fa-users'),(5,1,NULL,'权限','/admin/permission',29,'','','fa-key'),(6,1,2,'应用新增','/admin/app/edit',4,'\0','','fa-plus-circle blue'),(7,1,2,'应用禁用','/admin/app/enable',3,'\0','','fa-lock orange'),(8,1,2,'应用启用','/admin/app/enable',2,'\0','','fa-unlock green'),(9,1,2,'应用删除','/admin/app/delete',1,'\0','','fa-trash-o red'),(10,1,3,'管理员新增','/admin/user/edit',6,'\0','','fa-plus-circle blue'),(11,1,3,'管理员禁用','/admin/user/enable',5,'\0','','fa-lock orange'),(12,1,3,'管理员启用','/admin/user/enable',4,'\0','','fa-unlock green'),(13,1,3,'管理员删除','/admin/user/delete',3,'\0','','fa-trash-o red'),(14,1,3,'重置密码','/admin/user/resetPassword',2,'\0','','fa-key grey'),(15,1,3,'分配角色','/admin/user/allocate',1,'\0','','fa-cog grey'),(16,1,4,'角色新增','/admin/role/edit',5,'\0','','fa-plus-circle blue'),(17,1,4,'角色禁用','/admin/role/enable',4,'\0','','fa-lock orange'),(18,1,4,'角色启用','/admin/role/enable',3,'\0','','fa-unlock green'),(19,1,4,'角色删除','/admin/role/delete',2,'\0','','fa-trash-o red'),(20,1,4,'角色授权','/admin/role/allocate',1,'\0','','fa-cog grey'),(22,1,2,'应用列表','/admin/app/list',5,'\0','',''),(23,1,3,'管理员列表','/admin/user/list',7,'\0','',''),(24,1,4,'角色列表','/admin/role/list',6,'\0','',''),(25,1,5,'权限树列表','/admin/permission/nodes',1,'\0','',''),(26,1,2,'应用保存','/admin/app/save',1,'\0','',''),(27,1,3,'管理员保存','/admin/user/save',1,'\0','',''),(28,1,4,'角色保存','/admin/role/save',1,'\0','',''),(29,1,5,'权限保存','/admin/permission/save',1,'\0','',''),(30,1,5,'权限删除','/admin/permission/delete',1,'\0','',''),(33,81,NULL,'测试','/admin/demo',1,'\0','','fa-user'),(34,81,33,'测试列表','/admin/demo/list',1,'\0','',''),(35,81,33,'测试新增/修改','/admin/demo/edit',1,'\0','',''),(36,81,33,'测试删除','/admin/demo/delete',1,'\0','',''),(39,1,NULL,'导航栏','/admin/admin/menu',99,'\0','',''),(40,81,NULL,'导航栏','/admin/admin/menu',99,'\0','',''),(41,1,NULL,'个人中心','/admin/profile',89,'\0','','fa fa-desktop'),(42,1,41,'修改密码','/admin/profile/savePassword',1,'\0','','');
/*!40000 ALTER TABLE `SYS_PERMISSION` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_RE_ROLE_PERMISSION`
--

DROP TABLE IF EXISTS `SYS_RE_ROLE_PERMISSION`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_RE_ROLE_PERMISSION`
--

LOCK TABLES `SYS_RE_ROLE_PERMISSION` WRITE;
/*!40000 ALTER TABLE `SYS_RE_ROLE_PERMISSION` DISABLE KEYS */;
INSERT INTO `SYS_RE_ROLE_PERMISSION` VALUES (176,4,40,81),(179,4,33,81),(180,4,34,81),(181,4,35,81),(182,4,36,81),(212,1,39,1),(213,1,41,1),(214,1,42,1),(215,1,2,1),(216,1,22,1),(217,1,6,1),(218,1,7,1),(219,1,8,1),(220,1,9,1),(221,1,26,1),(222,1,3,1),(223,1,23,1),(224,1,10,1),(225,1,11,1),(226,1,12,1),(227,1,13,1),(228,1,14,1),(229,1,15,1),(230,1,27,1),(231,1,4,1),(232,1,24,1),(233,1,16,1),(234,1,17,1),(235,1,18,1),(236,1,19,1),(237,1,20,1),(238,1,28,1),(239,1,5,1),(240,1,25,1),(241,1,29,1),(242,1,30,1);
/*!40000 ALTER TABLE `SYS_RE_ROLE_PERMISSION` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_RE_USER_APP`
--

DROP TABLE IF EXISTS `SYS_RE_USER_APP`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_RE_USER_APP`
--

LOCK TABLES `SYS_RE_USER_APP` WRITE;
/*!40000 ALTER TABLE `SYS_RE_USER_APP` DISABLE KEYS */;
INSERT INTO `SYS_RE_USER_APP` VALUES (7,2,81),(8,2,1);
/*!40000 ALTER TABLE `SYS_RE_USER_APP` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_RE_USER_ROLE`
--

DROP TABLE IF EXISTS `SYS_RE_USER_ROLE`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_RE_USER_ROLE`
--

LOCK TABLES `SYS_RE_USER_ROLE` WRITE;
/*!40000 ALTER TABLE `SYS_RE_USER_ROLE` DISABLE KEYS */;
INSERT INTO `SYS_RE_USER_ROLE` VALUES (7,2,4,81),(8,2,1,1);
/*!40000 ALTER TABLE `SYS_RE_USER_ROLE` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_ROLE`
--

DROP TABLE IF EXISTS `SYS_ROLE`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_ROLE`
--

LOCK TABLES `SYS_ROLE` WRITE;
/*!40000 ALTER TABLE `SYS_ROLE` DISABLE KEYS */;
INSERT INTO `SYS_ROLE` VALUES (1,1,'单点登录管理员角色',999,'',''),(4,81,'demo管理员角色',1,'','');
/*!40000 ALTER TABLE `SYS_ROLE` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SYS_USER`
--

DROP TABLE IF EXISTS `SYS_USER`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SYS_USER`
--

LOCK TABLES `SYS_USER` WRITE;
/*!40000 ALTER TABLE `SYS_USER` DISABLE KEYS */;
INSERT INTO `SYS_USER` VALUES (2,'admin','26524bdf4ea266f131566a89e8f4972c',NULL,NULL,0,'2015-06-02 11:31:56','');
/*!40000 ALTER TABLE `SYS_USER` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-05-29  1:00:15
