# smart-sso
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://opensource.org/licenses/MIT)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/a466350665/smart-sso/pulls)
[![GitHub stars](https://img.shields.io/github/stars/a466350665/smart-sso.svg?style=social&label=Stars)](https://github.com/a466350665/smart-sso)
[![GitHub forks](https://img.shields.io/github/forks/a466350665/smart-sso.svg?style=social&label=Fork)](https://github.com/a466350665/smart-sso)

QQ交流群：454343484、769134727

## 简述
    smart-sso使用当下最流行的SpringBoot技术，基于OAuth2认证授权 + RBAC权限设计，为您构建一个轻量级、易理解、高可用、高扩展性的单点登录权限管理应用基层。

## 相关文档
- [smart-sso单点登录（一）：介绍](https://blog.csdn.net/a466350665/article/details/54140411)
- [smart-sso单点登录（二）：快速开始](https://blog.csdn.net/a466350665/article/details/79628553)
- [smart-sso单点登录（三）：接入指南](https://blog.csdn.net/a466350665/article/details/139736085)
- [smart-sso单点登录（四）：前后端分离](https://blog.csdn.net/a466350665/article/details/109742638)
- [smart-sso单点登录（五）：分布式部署](https://blog.csdn.net/a466350665/article/details/109388429)

## 功能说明

1. **轻量级：** 基于SpringBoot和OAuth2协议的授权码模式的极简认证授权实现；

2. **自动续约：** 使用OAuth2协议的accessToken失效机制，通过refreshToken刷新时自动更新服务端凭证时效，完成自动续约；

3. **按钮级权限控制：** 服务端对权限进行菜单和按钮分类，通过请求uri和请求方法匹配的方式实现权限按钮级控制；

4. **跨域支持：** 服务端和客户端允许在不同域名下，完成跨域的单点登录和退出机制；

5. **前后端分离支持：** 用户在前后端分离的架构下(无Cookie模式)，也能轻易实现单点登录和单点退出；

6. **分布式部署：** 服务端和客户端都支持多实例部署场景，基于redis实现分布式Token管理；

## 为何是OAuth2？

以下对常见的几种SSO认证方式对比：

| 特性               | 传统Token       | JWT                | OAuth2             |
|------------------|-----------------|--------------------|--------------------|
| 单点登录         | 支持            | 支持               | 支持               |
| 单点退出         | 支持            | 较难实现               | 支持               |
| 踢人下线         | 支持            | 较难实现               | 支持               |
| 自动续签         | 较难实现           | 支持                |支持|
| 安全性           | 一般              | 较好          | 高        |
| 性能             | 一般               | 高            | 较好      |

**解释：**
对于传统 Token 方式，机制相对简单直接。它通常由服务端生成一个随机字符串作为令牌，并在客户端和服务器之间传递来验证用户身份。
缺点也很明显，由于Token缺乏时效和刷新机制，较难实现自动续签的功能，客户端的请求也需要频繁调用服务端做验证，对于一些小型项目性能或安全性要求不是特别高的场景可能足够使用。


JWT由于其无状态的特性，服务端只需存储密钥，不存储Token信息，减少了服务端的存储压力。但在SSO场景下，较难实现单点退出和踢人功能，因为这些功能通常需要依靠后端存储Token实现，这与JWT的理念相悖，对于一些安全性要求高的项目，这些也是不可或缺的能力。


OAuth2常用于第三方应用的授权登录，你会发现它也完全契合SSO的场景，但实现难度相对偏高，它安全性最高的授权码模式需要在服务端实现授权码和Token的存储。同时，也天然具备JWT需要借助双Token方式才能实现的自动续签机制。而且每个客户端应用需要接入到OAuth2认证授权中心，必须要在其服务端登记时颁发的应用密钥信息（ClientId、ClientSecret）才能获取accessToken，这样做可以完成对用户身份（授权码获取阶段）和客户端应用身份（获取accessToken阶段）的双重校验保障。

**结论：**
smart-sso选择使用OAuth2来建设单点登录权限系统。

在客户端后端维护accessToken生命周期的管理，用户携带调用凭证的请求在客户端应用本地就完成校验，减少了客户端应用和服务端的不必要交互。

只有当客户端应用本地校验accessToken失效后，由客户端后端向服务端发调起refreshToken请求，重新生成accessToken后返回，同时延长服务端凭证存根的时效，从而完成自动续签功能。

对于单点登录权限系统而言，登录成功后的第一要务就是获取登录用户当前应用的权限信息。所以，服务端必须为用户的每个客户端应用都颁发accessToken，不能凭借仅从单一客户端应用获取的accessToken，就能获得认证授权中心管理的所有应用资源权限，这符合OAuth2的初衷。

## 技术选型

| 技术                   | 版本    | 说明             |
| ---------------------- | ------- | ---------------- |
| spring-boot             | 2.5.13   | 容器 + MVC框架     |
| spring-boot-starter-data-redis    | 2.5.13   | 分布式场景Token管理  |
| spring-boot-starter-freemarker | 2.5.13   | 模板引擎  |
| springfox-boot-starter      | 3.0.0   | 文档     |
| mybatis-plus-boot-starter           | 3.5.2   | ORM框架  |
| mysql-connector-java    | 8.0.28   | 数据库驱动  |
| httpclient    | 4.5.14   | 授权码认证，客户端和服务端通信  |

## 数据库模型
![](./images/smart-sso-pdm.jpg)

## 项目结构

```lua
smart-sso
├── smart-sso-demo -- 客户端示例
├── smart-sso-demo-h5 -- 前后端分离客户端示例
├── smart-sso-server -- 单点登录权限管理服务端
├── smart-sso-starter -- 依赖装配模块
│   ├── smart-sso-starter-base -- 公用的基础依赖装配
│   ├── smart-sso-starter-client -- 客户端依赖装配
│   ├── smart-sso-starter-client-redis -- 客户端依赖装配，分布式部署场景redis支持
│   ├── smart-sso-starter-server -- 服务端依赖装配
│   ├── smart-sso-starter-server-redis -- 服务端依赖装配，分布式部署场景redis支持
```

## 模块依赖关系

![](./images/smart-sso.png)

<font color="red">注：</font>  
1.红色实线可以理解为服务端也需要单点登录，同样是其自身的一个客户端；  
2.红色虚线表示无论是服务端还是客户端，当需要集群部署时，可选用Redis版本的依赖来实现Token共享；

## 单点登录原理
![](./images/smart-sso-login.png)


## 单点退出原理
![](./images/smart-sso-logout.png)


## 效果展示
### 单点登录页
![](./images/img1.png)

### 客户端示例登录成功页
![](./images/img2.png)

### 服务端管控页
![](./images/img3.png)

![](./images/img4.png)

![](./images/img5.png)

![](./images/img6.png)

### 服务端管控页手机端效果
![](./images/img10.jpg)

![](./images/img11.jpg)

![](./images/img12.jpg)

![](./images/img13.jpg)