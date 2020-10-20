package com.smart.sso.client.dto;

/**
 * RPC回传用户对象
 * 
 * @author Joe
 */
public class SessionUser extends AccessToken {

    private static final long serialVersionUID = 4507869346123296527L;

    /**
     * 当服务端session时间过半，延长服务端session
     */
    private long refreshTime;
    
    public SessionUser(long refreshTime, int timeout, String refreshToken, SsoUser user) {
        super(timeout, refreshToken, user);
        this.refreshTime = refreshTime;
    }

    public long getRefreshTime() {
        return refreshTime;
    }

    public void setRefreshTime(long refreshTime) {
        this.refreshTime = refreshTime;
    }
}