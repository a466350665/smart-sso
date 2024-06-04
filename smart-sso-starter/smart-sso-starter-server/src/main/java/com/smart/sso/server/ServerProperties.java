package com.smart.sso.server;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties("smart.sso.server")
public class ServerProperties {

    /**
     * 单点登录超时时间，默认2小时（单位秒）
     */
    private int timeout = 7200;

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }
}