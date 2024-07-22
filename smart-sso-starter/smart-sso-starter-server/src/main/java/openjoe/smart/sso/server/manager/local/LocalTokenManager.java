package openjoe.smart.sso.server.manager.local;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.server.entity.TokenContent;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地调用凭证管理
 *
 * @author Joe
 */
public class LocalTokenManager extends AbstractTokenManager implements ExpirationPolicy {

    private final Logger logger = LoggerFactory.getLogger(LocalTokenManager.class);
    private Map<String, ExpirationWrapper<String>> accessTokenMap = new ConcurrentHashMap<>();
    private Map<String, ExpirationWrapper<TokenContent>> refreshTokenMap = new ConcurrentHashMap<>();
    private Map<String, Set<String>> tgtMap = new ConcurrentHashMap<>();

    public LocalTokenManager(int accessTokenTimeout, int refreshTokenTimeout) {
        super(accessTokenTimeout, refreshTokenTimeout);
    }

    @Override
    public void create(String refreshToken, TokenContent tokenContent) {
        ExpirationWrapper<String> atWrapper = new ExpirationWrapper(refreshToken, getAccessTokenTimeout());
        accessTokenMap.put(tokenContent.getAccessToken(), atWrapper);

        ExpirationWrapper<TokenContent> rtWrapper = new ExpirationWrapper(tokenContent, getRefreshTokenTimeout());
        refreshTokenMap.put(refreshToken, rtWrapper);

        tgtMap.computeIfAbsent(tokenContent.getTgt(), a -> new HashSet<>()).add(refreshToken);
        logger.debug("调用凭证创建成功, accessToken:{}, refreshToken:{}", tokenContent.getAccessToken(), refreshToken);
    }

    @Override
    public TokenContent get(String refreshToken) {
        ExpirationWrapper<TokenContent> wrapper = refreshTokenMap.get(refreshToken);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        } else {
            return wrapper.getObject();
        }
    }

    @Override
    public TokenContent getByAccessToken(String accessToken) {
        ExpirationWrapper<String> wrapper = accessTokenMap.get(accessToken);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return get(wrapper.getObject());
    }

    @Override
    public void remove(String refreshToken) {
        // 删除refreshToken
        ExpirationWrapper<TokenContent> wrapper = refreshTokenMap.remove(refreshToken);
        if (wrapper == null) {
            return;
        }

        // 删除accessToken
        accessTokenMap.remove(wrapper.getObject().getAccessToken());

        // 删除tgt映射中的refreshToken
        Set<String> refreshTokenSet = tgtMap.get(wrapper.getObject().getTgt());
        if (CollectionUtils.isEmpty(refreshTokenSet)) {
            return;
        }
        refreshTokenSet.remove(refreshToken);
    }

    @Override
    public void removeByTgt(String tgt) {
        // 删除tgt映射中的refreshToken集合
        Set<String> refreshTokenSet = tgtMap.remove(tgt);
        if (CollectionUtils.isEmpty(refreshTokenSet)) {
            return;
        }
        refreshTokenSet.forEach(refreshToken -> {
            // 删除refreshToken
            ExpirationWrapper<TokenContent> wrapper = refreshTokenMap.remove(refreshToken);
            if (wrapper == null) {
                return;
            }
            TokenContent tokenContent = wrapper.getObject();
            if (tokenContent == null) {
                return;
            }

            // 删除accessToken
            accessTokenMap.remove(tokenContent.getAccessToken());

            // 发起客户端退出请求
            logger.debug("发起客户端退出请求, accessToken:{}, refreshToken:{}, url:{}", tokenContent.getAccessToken(), refreshToken, tokenContent.getRedirectUri());
            sendLogoutRequest(tokenContent.getRedirectUri(), tokenContent.getAccessToken());
        });
    }

    @Override
    public void verifyExpired() {
        accessTokenMap.forEach((accessToken, wrapper) -> {
            if (wrapper.checkExpired()) {
                accessTokenMap.remove(accessToken);
                logger.debug("调用凭证已失效, accessToken:{}", accessToken);
            }
        });

        refreshTokenMap.forEach((refreshToken, wrapper) -> {
            if (wrapper.checkExpired()) {
                remove(refreshToken);
                logger.debug("刷新凭证已失效, accessToken:{}, refreshToken:{}", wrapper.getObject().getAccessToken(), refreshToken);
            }
        });
    }
}
