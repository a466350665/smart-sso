# smart-sso
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://opensource.org/licenses/MIT)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/a466350665/smart-sso/pulls)
[![GitHub stars](https://img.shields.io/github/stars/a466350665/smart-sso.svg?style=social&label=Stars)](https://github.com/a466350665/smart-sso)
[![GitHub forks](https://img.shields.io/github/forks/a466350665/smart-sso.svg?style=social&label=Fork)](https://github.com/a466350665/smart-sso)

QQ交流群：454343484🈵、769134727

## 简述
    smart-sso使用当下最流行的SpringBoot技术，基于Oauth2认证授权协议，为您构建一个易理解、高可用、高扩展性的分布式单点登录应用基层。

## 相关文档
- [Java单点登录系统—简介（一）](https://blog.csdn.net/a466350665/article/details/54140411)
- [Java单点登录系统—部署文档（二）](http://blog.csdn.net/a466350665/article/details/79628553)
- [Java单点登录系统—App登录支持（三）](https://blog.csdn.net/a466350665/article/details/109742638)
- [Java单点登录系统—引入redis支持分布式（四）](https://blog.csdn.net/a466350665/article/details/109388429)

## 组织结构

```lua
smart-sso
├── smart-sso-client -- 客户端依赖包
├── smart-sso-client-redis -- 客户端依赖包，分布式redis支持
├── smart-sso-demo -- 客户端
├── smart-sso-server -- 服务端
```

## 技术选型
- JDK：1.8+
- 项目构建工具：Maven 3.3.3
- MVC框架：SpringBoot 2.1.0.RELEASE
- JSON工具：Fastjson 1.2.69
- 日志管理：SLF4J 1.7.21
- 单点登录：基于Oauth2协议实现

## 单点登录原理
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201030140826191.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)


## 单点退出原理
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201030141044187.jpg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)


## 效果展示
### 单点登录页
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201030163204421.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)

### 服务端登录成功页
![在这里插入图片描述](https://img-blog.csdnimg.cn/20201030163112313.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)

### 客户端登录成功页
![](https://img-blog.csdnimg.cn/20201020163349855.png?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2E0NjYzNTA2NjU=,size_16,color_FFFFFF,t_70#pic_center)