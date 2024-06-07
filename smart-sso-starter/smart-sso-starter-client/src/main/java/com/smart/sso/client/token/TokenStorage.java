package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.base.entity.Result;
import com.smart.sso.client.ClientProperties;
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

    protected static class TokenWrapper extends ObjectWrapper<AccessToken> {

        private Long refreshExpired;

        public TokenWrapper(){
            super();
        }

        public TokenWrapper(AccessToken at, Long expired, Long refreshExpired) {
            super(at, expired);
            this.refreshExpired = refreshExpired;
        }

        public long getRefreshExpired() {
            return refreshExpired;
        }

        public void setRefreshExpired(Long refreshExpired) {
            this.refreshExpired = refreshExpired;
        }

        public boolean verifyExpired() {
            return System.currentTimeMillis() > getExpired();
        }

        public boolean verifyRefreshExpired() {
            return System.currentTimeMillis() > getRefreshExpired();
        }
    }
}
