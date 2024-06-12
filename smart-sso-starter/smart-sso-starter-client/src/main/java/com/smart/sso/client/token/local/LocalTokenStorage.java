package com.smart.sso.client.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;

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

    @Override
    public void create(String st, TokenWrapper wrapper) {
        stMap.put(st, wrapper);
        tokenMap.put(wrapper.getObject().getAccessToken(), st);
        logger.info("服务凭证生成成功, accessToken:{}", wrapper.getObject().getAccessToken());
    }

    @Override
    public TokenWrapper get(String st) {
        return stMap.get(st);
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
