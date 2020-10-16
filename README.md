# smart-sso
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://opensource.org/licenses/MIT)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/a466350665/smart-sso/pulls)
[![GitHub stars](https://img.shields.io/github/stars/a466350665/smart-sso.svg?style=social&label=Stars)](https://github.com/a466350665/smart-sso)
[![GitHub forks](https://img.shields.io/github/forks/a466350665/smart-sso.svg?style=social&label=Fork)](https://github.com/a466350665/smart-sso)

QQ交流群：454343484🈵、769134727

## 简述
    smart-sso使用当下最流行的SpringBoot技术，参考并简化Cas中心认证服务协议，为您构建一个易理解、高可用、高扩展性的单点登录应用基层。
 
## 部署文档
- [Java单点登录系统（一）—简介](https://blog.csdn.net/a466350665/article/details/54140411)
- [Java单点登录系统（二）—部署文档](http://blog.csdn.net/a466350665/article/details/79628553)

## 组织结构

```lua
smart-sso
├── smart-sso-client -- 客户端依赖包
├── smart-sso-demo -- 客户端
├── smart-sso-server -- 服务端
```

## 技术选型
- JDK：1.8+
- 项目构建工具：Maven 3.3.3
- MVC框架：SpringBoot 2.1.0.RELEASE
- JSON工具：Fastjson 1.2.69
- 日志管理：SLF4J 1.7.21
- 单点登录：极简参考Cas协议实现

## 对比Cas + Shiro

### 请求协议
- Cas：默认https，如果使用http需要把cas server解压修改配置参数。<br>
- Smart：默认http，可选配为Https，减少配置，降低门槛。

### 配置层面
- Cas：在web.xml中指定的TicketValidationFilter、AuthenticationFilter及SingleSignOutFilter存在重复的serverName参数，serverName的修改，需要分别修改三处。<br>
- Smart：将serverName定义在properties文件中，通过Spring的<context:property-placeholder />标签注入，简化配置。(毕竟当今大部分Java项目都会用到Spring框架)

### 分布式部署
- Shiro：通常大家部署依赖Shiro注入的RedisSession。<br>
- Smart：通过大家更为常用的Spring，覆盖HttpSession注入RedisSession。

注：Cas和Shiro的风光伟绩就无需笔者在此多加吹捧，现在只是关起门来聊聊Smart的优势，当然个人的理解和知识面也有限，有描述不对的地方，也欢迎大家加群探讨。友善！勿喷！谢谢！


## 单点登录原理
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201015165855788.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)

## 单点退出原理
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201015173104361.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)


## 效果展示
### 单点登录页
![](https://img-blog.csdnimg.cn/20201015151854846.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)


### 服务端登录成功页
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201015152254717.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)

### 客户端登录成功页
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201015180153128.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)


## 作者寄语
艺术的做好一件擅长的事情