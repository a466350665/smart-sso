package com.smart.sso.client;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.core.Ordered;

@ConfigurationProperties("smart.sso")
public class ClientProperties {

    /**
     * 服务端地址
     */
    private String serverUrl;

    /**
     * 应用Id
     */
    private String appId;

    /**
     * 应用密钥
     */
    private String appSecret;

    /**
     * 拦截URL，默认全路径
     */
    private String[] urlPatterns = {"/*"};

    /**
     * 忽略拦截URL
     */
    private String[] excludeUrls;

    /**
     * 排序，默认最高优先级
     */
    private int order = Ordered.HIGHEST_PRECEDENCE;

    /**
     * 客户端拦截Filter容器名称
     */
    private String containerName = "clientContainer";

    public String getServerUrl() {
        return serverUrl;
    }

    public void setServerUrl(String serverUrl) {
        this.serverUrl = serverUrl;
    }

    public String getAppId() {
        return appId;
    }

    public void setAppId(String appId) {
        this.appId = appId;
    }

    public String getAppSecret() {
        return appSecret;
    }

    public void setAppSecret(String appSecret) {
        this.appSecret = appSecret;
    }

    public String[] getUrlPatterns() {
        return urlPatterns;
    }

    public void setUrlPatterns(String[] urlPatterns) {
        this.urlPatterns = urlPatterns;
    }

    public String[] getExcludeUrls() {
        return excludeUrls;
    }

    public void setExcludeUrls(String[] excludeUrls) {
        this.excludeUrls = excludeUrls;
    }

    public int getOrder() {
        return order;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public String getContainerName() {
        return containerName;
    }

    public void setContainerName(String containerName) {
        this.containerName = containerName;
    }
}