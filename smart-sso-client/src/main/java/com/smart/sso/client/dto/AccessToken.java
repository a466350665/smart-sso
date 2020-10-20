package com.smart.sso.client.dto;

import java.io.Serializable;

/**
 * 服务端回传Token对象
 * 
 * @author Joe
 */
public class AccessToken implements Serializable {

    private static final long serialVersionUID = 4507869346123296527L;

    /**
     * 当前Token时效
     */
    private int timeout;
    /**
     * 当前Token超时，用于刷新Token并延长服务端session时效必要参数
     */
    private String refreshToken;
    /**
     * 自定义远程认证回传给客户端的用户信息
     */
    private SsoUser user;
    
    public AccessToken(int timeout, String refreshToken, SsoUser user) {
    	super();
        this.timeout = timeout;
        this.refreshToken = refreshToken;
        this.user = user;
    }

    public SsoUser getUser() {
        return user;
    }

    public void setUser(SsoUser user) {
        this.user = user;
    }
    
    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }
}