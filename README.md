# Smart-SSO
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://opensource.org/licenses/MIT)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/a466350665/smart-sso/pulls)
[![GitHub stars](https://img.shields.io/github/stars/a466350665/smart-sso.svg?style=social&label=Stars)](https://github.com/a466350665/smart-sso)
[![GitHub forks](https://img.shields.io/github/forks/a466350665/smart-sso.svg?style=social&label=Fork)](https://github.com/a466350665/smart-sso)

QQ交流群：454343484、769134727

## 简述
    Smart-SSO使用当下最流行的SpringBoot技术，基于OAuth2认证授权 + RBAC权限设计，为您构建一个轻量级、易理解、高可用、高扩展性的单点登录权限管理应用基层。

## 相关文档
- [Smart-SSO单点登录（一）：介绍](https://blog.csdn.net/a466350665/article/details/54140411)
- [Smart-SSO单点登录（二）：快速开始](https://blog.csdn.net/a466350665/article/details/79628553)
- [Smart-SSO单点登录（三）：接入指南](https://blog.csdn.net/a466350665/article/details/139736085)
- [Smart-SSO单点登录（四）：前后端分离](https://blog.csdn.net/a466350665/article/details/109742638)
- [Smart-SSO单点登录（五）：分布式部署](https://blog.csdn.net/a466350665/article/details/109388429)

## 功能说明

1. **轻量级：** 基于SpringBoot和OAuth2协议的授权码模式的极简认证授权实现；

2. **单点退出：** 服务端支持远程通知所有客户端应用注销本地Token的退出机制；

3. **自动续约：** 使用OAuth2协议的accessToken失效机制，refreshToken刷新时自动更新服务端凭证时效，完成自动续约；

4. **按钮级权限控制：** 服务端对权限进行菜单和按钮分类，通过请求uri和请求方法匹配的方式实现权限按钮级控制；

5. **跨域支持：** 服务端和客户端允许在不同域名下，完成跨域的单点登录和退出机制；

6. **前后端分离支持：** 用户在前后端分离的架构下(无Cookie模式)，也能轻易实现单点登录和单点退出；

7. **分布式部署：** 服务端和客户端都支持多实例部署场景，基于redis实现分布式Token管理；

## 为何是OAuth2？

以下对常见的几种SSO认证方式对比：

| 特性               | 传统Token       | JWT                | OAuth2             |
|------------------|-----------------|--------------------|--------------------|
| 单点登录         | 支持            | 支持               | 支持               |
| 单点退出         | 支持            | 较难实现               | 支持               |
| 踢人下线         | 支持            | 较难实现               | 支持               |
| 自动续签         | 较难实现           | 支持                |支持|
| 性能             | 一般               | 高            | 较好      |
| 安全性           | 一般              | 较好          | 高        |

**解释：**   
对于传统 Token 方式，机制相对简单。它通常由服务端生成一个随机字符串作为令牌，并在客户端和服务器之间传递来验证用户身份。
缺点也很明显，由于缺乏时效和刷新机制，较难实现自动续签功能，用户发到客户端后端的请求也需要频繁调用服务端做Token校验，对于一些小型项目性能或安全性要求不是特别高的场景也可能足够使用。

JWT由于其无状态的特性，服务端只需存储密钥，不存储Token信息，减少了服务端的存储压力。但在SSO场景下，不易实现单点退出和踢人下线的功能，这些功能通常需要依靠后端存储Token结合注销远程通知或共享存储实现，这与JWT的理念相悖，对于一些安全性要求很高的项目，这些也是不可或缺的能力。

OAuth2常用于第三方应用的授权登录，它也完全契合SSO的场景，只是实现难度相对偏高。它天然具备Token的时效和刷新机制，可以实现的Token的续签，而JWT需要改进为双Token方式才能完成。对每个需要接入到OAuth2认证授权中心的应用，必须要在其服务端进行登记，并颁发密钥信息（ClientId、ClientSecret），有了它们Token才能按照流程被获取，这样做就可以完成对用户身份（授权码获取阶段）和客户端应用身份（获取accessToken阶段）的双重校验保障。
对于认证授权系统而言，登录成功后的第一要务就是获取登录用户当前应用的权限信息，所以服务端必须为用户的每个客户端应用都分别颁发Token，不能凭借仅从单一客户端应用获取的Token，就能获得认证授权中心管理的所有应用资源权限，这也符合OAuth2的初衷。

**结论：**   
Smart-SSO选择使用OAuth2来建设，而且做了一些优化。比如客户端后端也缓存了Token，用户携带Token的请求在客户端应用本地就完成校验，大大减少了客户端应用和服务端的交互。续签机制也做了优化，客户端本地Token失效后，由客户端后端向服务端发起refreshToken请求，重新生成Token后写回前端，同时延长服务端凭证存根的时效，完成自动续签功能。

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