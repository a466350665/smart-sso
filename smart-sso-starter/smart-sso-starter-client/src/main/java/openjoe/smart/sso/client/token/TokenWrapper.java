package openjoe.smart.sso.client.token;

import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.TokenPermission;

public class TokenWrapper extends ExpirationWrapper<Token> {

    /**
     * 权限信息
     */
    private TokenPermission tokenPermission;

    /**
     * refreshToken过期时间
     */
    private Long refreshExpired;

    public TokenWrapper() {
        super();
    }

    public TokenWrapper(Token token, TokenPermission tokenPermission, int expiresIn, int refreshExpiresIn) {
        super(token, expiresIn);
        this.tokenPermission = tokenPermission;
        this.refreshExpired = System.currentTimeMillis() + refreshExpiresIn * 1000;
    }

    public TokenPermission getTokenPermission() {
        return tokenPermission;
    }

    public void setTokenPermission(TokenPermission tokenPermission) {
        this.tokenPermission = tokenPermission;
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