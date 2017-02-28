# Smart
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](http://opensource.org/licenses/MIT)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/a466350665/smart/pulls)
[![GitHub forks](https://img.shields.io/github/forks/shuzheng/zheng.svg?style=social&label=Fork)](https://github.com/a466350665/smart)

## 简述：
    Smart定位于用当下最流行的技术，为您构建一个易理解、高可用、高扩展性的应用基层，帮助实现快速开发。
    内置Java基础工具类、Dubbo服务治理、单点登录权限系统(按钮级，权限修改实时生效)、支持分布式的定时任务服务及代码生成器，
    和一套易用高兼容的boostrap前端Html模板。
    
## 技术选型

### 浏览器兼容：
    Firefox 5+, Google Chrome 14+,Internet Explorer 8,Internet Explorer 9,Opera 11,Safari 5及各种手机浏览器
### 前端：
    Html5 + Css3.0 + Bootstrap(ACE模板) + Jquery
### 后端：
    Maven+SpringMVC+Spring+Mybatis/Hibernate+Zookeeper+Dubbo+Sso+Redis(选配)+Druid+Fastjson+Mysql+ActiveMQ(选配)
### 1.单点登录（Sso）;
    简单实用的基于Cookie实现的单点登录
### 2.分布式服务治理（Dubbo）;
    一个分布式服务框架,致力于提供高性能和透明化的RPC远程服务调用方案,是阿里巴巴SOA服务化治理方案的核心框架
### 3.数据库连接池(Druid);
    数据库连接池阿里巴巴的 druid。Druid在监控、可扩展性、稳定性和性能方面都有明显的优势
### 4.Json处理工具包(Fastjson);
    fastjson 是一个性能很好的 Java 语言实现的 JSON 解析器和生成器
### 5.开源消息总线 (ActiveMQ);
    对Spring的支持,通过MQ监听权限修改消息通知
### 6.Nosql(Redis) ;
    Redis是一个高性能的key-value数据库，最流行的共享Session处理方案
    
## 效果展示：

### 数据库模型
![数据库模型](http://img.blog.csdn.net/20170228162027225?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)

### 代码生成器
![代码生成器](http://img.blog.csdn.net/20170228160358030?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170118203537172?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170119151157271?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172009071?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172032962?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172050728?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172102416?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)

### 手机浏览器效果
![](http://img.blog.csdn.net/20170106172646403?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172905092?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172915803?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
![](http://img.blog.csdn.net/20170106172926694?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvYTQ2NjM1MDY2NQ==/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)
