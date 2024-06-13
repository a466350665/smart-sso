package com.smart.sso.client.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Token存储本地实现
 *
 * @author Joe
 */
public final class LocalTokenStorage implements TokenStorage, ExpirationPolicy {
    private final Logger logger = LoggerFactory.getLogger(LocalTokenStorage.class);
    private final Map<String, TokenWrapper> tokenMap = new ConcurrentHashMap<>();

    @Override
    public void create(String accessToken, TokenWrapper wrapper) {
        tokenMap.put(accessToken, wrapper);
        logger.info("服务凭证生成成功, accessToken:{}", accessToken);
    }

    @Override
    public TokenWrapper get(String accessToken) {
        return tokenMap.get(accessToken);
    }

    @Override
    public void remove(String accessToken) {
        tokenMap.remove(accessToken);
    }

    @Override
    public void verifyExpired() {
        tokenMap.forEach((accessToken, wrapper) -> {
            if (wrapper.checkRefreshExpired()) {
                remove(accessToken);
                logger.info("服务凭证已失效, accessToken:{}", accessToken);
            }
        });
    }
}
