package com.smart.sso.client.token.local;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.token.TokenStorage;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Token存储本地实现
 * 
 * @author Joe
 */
public final class LocalTokenStorage extends TokenStorage implements ExpirationPolicy {
    private final Map<String, TokenWrapper> tokenMap = new ConcurrentHashMap<>();

    public LocalTokenStorage(ClientProperties properties) {
        this.properties = properties;
    }

    @Override
    public void create(AccessToken at) {
        tokenMap.put(at.getAccessToken(), createTokenWrapper(at));
        logger.info("服务凭证生成成功, accessToken:{}", at.getAccessToken());
    }

    @Override
    public AccessToken getAndRefresh(String accessToken) {
        TokenWrapper wrapper = tokenMap.get(accessToken);
        if (wrapper == null) {
            return null;
        }
        // accessToken没过期直接返回
        if(!wrapper.checkExpired()){
            return wrapper.getObject();
        }
        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        if(!wrapper.checkRefreshExpired()){
            AccessToken at = refreshToken(wrapper.getObject().getRefreshToken());
            if(at != null){
                create(at);
                return at;
            }
        }
        return null;
    }

    @Override
    public void remove(String accessToken) {
        tokenMap.remove(accessToken);
        logger.debug("服务凭证删除成功, accessToken:{}", accessToken);
    }

    @Override
    public void verifyExpired() {
        tokenMap.forEach((accessToken, wrapper) -> {
            if (wrapper.checkRefreshExpired()) {
                tokenMap.remove(accessToken);
                logger.debug("服务凭证已失效, accessToken:{}", accessToken);
            }
        });
    }
}
