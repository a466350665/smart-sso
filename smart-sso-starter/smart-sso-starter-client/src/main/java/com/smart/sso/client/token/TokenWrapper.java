package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.ExpirationWrapper;

public class TokenWrapper extends ExpirationWrapper<AccessToken> {

    /**
     * refreshToken过期时间
     */
    private Long refreshExpired;

    public TokenWrapper() {
        super();
    }

    public TokenWrapper(AccessToken at, int expiresIn, int refreshExpiresIn) {
        super(at, expiresIn);
        this.refreshExpired = System.currentTimeMillis() + refreshExpiresIn * 1000;
    }

    public long getRefreshExpired() {
        return refreshExpired;
    }

    public void setRefreshExpired(Long refreshExpired) {
        this.refreshExpired = refreshExpired;
    }

    /**
     * 校验refreshToken是否过期
     *
     * @return
     */
    public boolean checkRefreshExpired() {
        return System.currentTimeMillis() > getRefreshExpired();
    }
}