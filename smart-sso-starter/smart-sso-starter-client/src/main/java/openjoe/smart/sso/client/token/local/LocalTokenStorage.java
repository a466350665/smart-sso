package openjoe.smart.sso.client.token.local;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.TokenWrapper;
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
    private final Map<String, TokenWrapper> accessTokenMap = new ConcurrentHashMap<>();
    private Map<String, ExpirationWrapper<String>> refreshTokenMap = new ConcurrentHashMap<>();

    @Override
    public void create(String accessToken, TokenWrapper wrapper) {
        accessTokenMap.put(accessToken, wrapper);
        refreshTokenMap.put(wrapper.getObject().getRefreshToken(), new ExpirationWrapper<>(accessToken, wrapper.getObject().getRefreshExpiresIn()));
        logger.debug("服务凭证创建成功, accessToken:{}", accessToken);
    }

    @Override
    public TokenWrapper get(String accessToken) {
        return accessTokenMap.get(accessToken);
    }

    @Override
    public void remove(String accessToken) {
        TokenWrapper wrapper = accessTokenMap.remove(accessToken);
        if (wrapper == null) {
            return;
        }
        refreshTokenMap.remove(wrapper.getObject().getRefreshToken());
    }

    @Override
    public void verifyExpired() {
        accessTokenMap.forEach((accessToken, wrapper) -> {
            if (wrapper.checkRefreshExpired()) {
                remove(accessToken);
                logger.debug("服务凭证已失效, accessToken:{}", accessToken);
            }
        });
    }

    @Override
    public String getAccessToken(String refreshToken) {
        ExpirationWrapper<String> wrapper = refreshTokenMap.get(refreshToken);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }
}
