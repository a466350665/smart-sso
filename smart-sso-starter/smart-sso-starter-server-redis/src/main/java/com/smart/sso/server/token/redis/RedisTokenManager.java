package com.smart.sso.server.token.redis;

import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.entity.TokenContent;
import com.smart.sso.server.token.AbstractTokenManager;
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
    private static final String REFRESH_TOKEN_KEY = "server_rt_";
    private static final String TGT_REFRESH_TOKEN_KEY = "server_tgt_rt_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenManager(int accessTokenTimeout, int refreshTokenTimeout, StringRedisTemplate redisTemplate) {
        super(accessTokenTimeout, refreshTokenTimeout);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String refreshToken, TokenContent tokenContent) {
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
        String atcStr = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
        if (!StringUtils.hasLength(atcStr)) {
            return null;
        }
        return JsonUtils.parseObject(atcStr, TokenContent.class);
    }

    @Override
    public void remove(String refreshToken) {
        String atcStr = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
        if (!StringUtils.hasLength(atcStr)) {
            return;
        }
        redisTemplate.delete(refreshToken);

        // TGT集合中删除当前refreshToken
        TokenContent tokenContent = JsonUtils.parseObject(atcStr, TokenContent.class);
        if (tokenContent == null) {
            return;
        }
        redisTemplate.opsForSet().remove(TGT_REFRESH_TOKEN_KEY + tokenContent.getTgt(), refreshToken);
    }

    @Override
    public void removeByTgt(String tgt) {
        Set<String> accessTokenSet = redisTemplate.opsForSet().members(TGT_REFRESH_TOKEN_KEY + tgt);
        if (CollectionUtils.isEmpty(accessTokenSet)) {
            return;
        }
        redisTemplate.delete(TGT_REFRESH_TOKEN_KEY + tgt);

        accessTokenSet.forEach(refreshToken -> {
            String atcStr = redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
            if (!StringUtils.hasLength(atcStr)) {
                return;
            }
            redisTemplate.delete(REFRESH_TOKEN_KEY + refreshToken);

            TokenContent tokenContent = JsonUtils.parseObject(atcStr, TokenContent.class);
            if (tokenContent == null) {
                return;
            }
            logger.debug("发起客户端退出请求, accessToken:{}, refreshToken:{}, url:{}", tokenContent.getAccessToken(), refreshToken, tokenContent.getRedirectUri());
            sendLogoutRequest(tokenContent.getRedirectUri(), tokenContent.getAccessToken());
        });
    }
}
