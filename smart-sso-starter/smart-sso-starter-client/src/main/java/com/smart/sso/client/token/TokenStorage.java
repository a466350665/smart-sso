package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.entity.Result;
import com.smart.sso.client.util.Oauth2Utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Token管理
 * 
 * @author Joe
 */
public abstract class TokenStorage {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    protected ClientProperties properties;

    /**
     * 登录成功后，存储accessToken
     *
     * @param at
     * @return
     */
    public abstract void create(AccessToken at);

    /**
     * 验证accessToken是否存在且在有效期内，过期使用refresh接口刷新
     *
     * @param accessToken
     * @return
     */
    public abstract AccessToken getAndRefresh(String accessToken);

    /**
     * 移除
     *
     * @param accessToken
     */
    public abstract void remove(String accessToken);

    /**
     * 通过refreshToken参数调用http请求延长服务端Token时效，并返回新的accessToken
     *
     * @param refreshToken
     * @return
     */
    protected AccessToken refreshToken(String refreshToken) {
        Result<AccessToken> result = Oauth2Utils.refreshToken(properties.getServerUrl(), properties.getAppId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("refreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }

    protected TokenWrapper createTokenWrapper(AccessToken at) {
        return new TokenWrapper(at, System.currentTimeMillis() + at.getExpiresIn() * 1000,
                System.currentTimeMillis() + at.getRefreshExpiresIn() * 1000);
    }

    protected static class TokenWrapper {
        private AccessToken at;
        private long expired;
        private long refreshExpired;

        public TokenWrapper(){
        }

        public TokenWrapper(AccessToken at, long expired, long refreshExpired) {
            super();
            this.at = at;
            this.expired = expired;
            this.refreshExpired = refreshExpired;
        }

        public AccessToken getAt() {
            return at;
        }

        public void setAt(AccessToken at) {
            this.at = at;
        }

        public long getExpired() {
            return expired;
        }

        public void setExpired(long expired) {
            this.expired = expired;
        }

        public long getRefreshExpired() {
            return refreshExpired;
        }

        public void setRefreshExpired(long refreshExpired) {
            this.refreshExpired = refreshExpired;
        }

        public boolean isExpired() {
            return System.currentTimeMillis() > expired;
        }

        public boolean isRefreshExpired() {
            return System.currentTimeMillis() > refreshExpired;
        }
    }
}
