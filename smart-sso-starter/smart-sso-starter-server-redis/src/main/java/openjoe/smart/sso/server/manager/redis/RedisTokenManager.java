package openjoe.smart.sso.server.manager.redis;

import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.server.entity.TokenContent;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * 分布式调用凭证管理
 *
 * @author Joe
 */
public class RedisTokenManager extends AbstractTokenManager {

    private final Logger logger = LoggerFactory.getLogger(RedisTokenManager.class);
    private static final String ACCESS_TOKEN_KEY = "server_at_";
    private static final String REFRESH_TOKEN_KEY = "server_rt_";
    private static final String TGT_REFRESH_TOKEN_KEY = "server_tgt_rt_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenManager(int accessTokenTimeout, int refreshTokenTimeout, int threadPoolSize, StringRedisTemplate redisTemplate) {
        super(accessTokenTimeout, refreshTokenTimeout, threadPoolSize);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String refreshToken, TokenContent tokenContent) {
        redisTemplate.opsForValue().set(ACCESS_TOKEN_KEY + tokenContent.getAccessToken(), refreshToken, getAccessTokenTimeout(),
                TimeUnit.SECONDS);

        redisTemplate.opsForValue().set(REFRESH_TOKEN_KEY + refreshToken, JsonUtils.toString(tokenContent), getRefreshTokenTimeout(),
                TimeUnit.SECONDS);

        redisTemplate.opsForSet().add(TGT_REFRESH_TOKEN_KEY + tokenContent.getTgt(), refreshToken);
        // 创建任意的Token，都为TGT和Token映射更新失效时间
        redisTemplate.expire(TGT_REFRESH_TOKEN_KEY + tokenContent.getTgt(), getRefreshTokenTimeout(),
                TimeUnit.SECONDS);
        logger.debug("Redis调用凭证创建成功, accessToken:{}, refreshToken:{}", tokenContent.getAccessToken(), refreshToken);
    }

    @Override
    public TokenContent get(String refreshToken) {
        String tc = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
        if (!StringUtils.hasLength(tc)) {
            return null;
        }
        return JsonUtils.parseObject(tc, TokenContent.class);
    }

    @Override
    public TokenContent getByAccessToken(String accessToken) {
        String refreshToken = redisTemplate.opsForValue().get(ACCESS_TOKEN_KEY + accessToken);
        if (!StringUtils.hasLength(refreshToken)) {
            return null;
        }
        return get(refreshToken);
    }

    @Override
    public void remove(String refreshToken) {
        String tc = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
        if (!StringUtils.hasLength(tc)) {
            return;
        }
        // 删除refreshToken
        redisTemplate.delete(REFRESH_TOKEN_KEY + refreshToken);

        TokenContent tokenContent = JsonUtils.parseObject(tc, TokenContent.class);
        if (tokenContent == null) {
            return;
        }
        // 删除accessToken
        redisTemplate.delete(ACCESS_TOKEN_KEY + tokenContent.getAccessToken());

        // 删除tgt映射中的refreshToken
        redisTemplate.opsForSet().remove(TGT_REFRESH_TOKEN_KEY + tokenContent.getTgt(), refreshToken);
    }

    @Override
    public void removeByTgt(String tgt) {
        Set<String> refreshTokenSet = redisTemplate.opsForSet().members(TGT_REFRESH_TOKEN_KEY + tgt);
        if (CollectionUtils.isEmpty(refreshTokenSet)) {
            return;
        }
        // 删除tgt映射中的refreshToken集合
        redisTemplate.delete(TGT_REFRESH_TOKEN_KEY + tgt);

        submitRemoveToken(refreshTokenSet);
    }

    @Override
    public void processRemoveToken(String refreshToken) {
        String tc = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
        if (!StringUtils.hasLength(tc)) {
            return;
        }
        // 删除refreshToken
        redisTemplate.delete(REFRESH_TOKEN_KEY + refreshToken);

        TokenContent tokenContent = JsonUtils.parseObject(tc, TokenContent.class);
        if (tokenContent == null) {
            return;
        }
        // 删除accessToken
        redisTemplate.delete(ACCESS_TOKEN_KEY + tokenContent.getAccessToken());

        // 发起客户端退出请求
        logger.debug("发起客户端退出请求, accessToken:{}, refreshToken:{}, logoutUri:{}", tokenContent.getAccessToken(), refreshToken, tokenContent.getLogoutUri());
        sendLogoutRequest(tokenContent.getLogoutUri(), tokenContent.getAccessToken());
    }

}
