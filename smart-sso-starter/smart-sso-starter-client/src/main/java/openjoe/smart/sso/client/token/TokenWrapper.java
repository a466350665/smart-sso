package openjoe.smart.sso.client.token;

import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.base.entity.Token;

import java.util.HashMap;
import java.util.Map;

public class TokenWrapper extends ExpirationWrapper<Token> {

    /**
     * refreshToken过期时间
     */
    private Long refreshExpired;

    /**
     * 用于扩展本地存储，类似HttpSession中的attributes
     */
    private Map<String, Object> attributes = new HashMap<>();

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

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public void setAttributes(Map<String, Object> attributes) {
        this.attributes = attributes;
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