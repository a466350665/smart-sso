package openjoe.smart.sso.client.token;

import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.ExpirationWrapper;

public class TokenWrapper extends ExpirationWrapper<Token> {

    /**
     * refreshToken过期时间
     */
    private Long refreshExpired;

    public TokenWrapper() {
        super();
    }

    public TokenWrapper(Token token, int expiresIn, int refreshExpiresIn) {
        super(token, expiresIn);
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