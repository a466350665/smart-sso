package com.smart.sso.client.token.local;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import com.smart.sso.client.util.Oauth2Utils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Token存储本地实现
 * 
 * @author Joe
 */
public final class LocalTokenStorage extends TokenStorage implements ExpirationPolicy {
    private final Map<String, TokenWrapper> stMap = new ConcurrentHashMap<>();
    private final Map<String, String> tokenMap = new ConcurrentHashMap<>();

    private ClientProperties properties;

    public LocalTokenStorage(ClientProperties properties) {
        this.properties = properties;
    }

    @Override
    public void create(String st, AccessToken at) {
        TokenWrapper wrapper = new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn());
        stMap.put(st, wrapper);
        tokenMap.put(at.getAccessToken(), st);
        logger.info("服务凭证生成成功, accessToken:{}", at.getAccessToken());
    }

    @Override
    public AccessToken get(String st) {
        TokenWrapper wrapper = stMap.get(st);
        if (wrapper == null) {
            return null;
        }
        // accessToken没过期直接返回
        if(!wrapper.checkExpired()){
            return wrapper.getObject();
        }
        return null;
    }

    @Override
    public AccessToken getAndRefresh(String st) {
        TokenWrapper wrapper = stMap.get(st);
        if (wrapper == null) {
            return null;
        }
        // accessToken没过期直接返回
        if(!wrapper.checkExpired()){
            return wrapper.getObject();
        }
        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        if(!wrapper.checkRefreshExpired()){
            AccessToken at = Oauth2Utils.getRefreshToken(properties, wrapper.getObject().getRefreshToken());
            if(at != null){
                remove(st);
                create(st, at);
                return at;
            }
        }
        return null;
    }

    @Override
    public void remove(String st) {
        TokenWrapper wrapper = stMap.remove(st);
        if (wrapper != null) {
            tokenMap.remove(wrapper.getObject().getAccessToken());
        }
    }

    @Override
    public void removeByAccessToken(String accessToken) {
        String st = tokenMap.remove(accessToken);
        stMap.remove(st);
    }

    @Override
    public void verifyExpired() {
        stMap.forEach((st, wrapper) -> {
            if (wrapper.checkRefreshExpired()) {
                stMap.remove(st);
                tokenMap.remove(wrapper.getObject().getAccessToken());
                logger.info("服务凭证已失效, accessToken:{}", wrapper.getObject().getAccessToken());
            }
        });
    }
}
