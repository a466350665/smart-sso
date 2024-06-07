package com.smart.sso.client.token.redis;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.token.TokenStorage;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * Token存储Redis实现
 *
 * @author Joe
 */
public final class RedisTokenStorage extends TokenStorage {

    private static final String TOKEN_KEY = "token_key_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenStorage(ClientProperties properties, StringRedisTemplate redisTemplate) {
        this.properties = properties;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(AccessToken at) {
        redisTemplate.opsForValue().set(TOKEN_KEY + at.getAccessToken(), JsonUtils.toJSONString(createTokenWrapper(at)),
                at.getRefreshExpiresIn(), TimeUnit.SECONDS);
    }

    @Override
    public AccessToken getAndRefresh(String accessToken) {
        String str = redisTemplate.opsForValue().get(TOKEN_KEY + accessToken);
        if (StringUtils.isEmpty(str)) {
            return null;
        }
        TokenWrapper wrapper = JsonUtils.parseObject(str, TokenWrapper.class);
        // accessToken没过期直接返回
        if (!wrapper.isExpired()) {
            return wrapper.getObject();
        }

        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        AccessToken at = refreshToken(wrapper.getObject().getRefreshToken());
        if (at != null) {
            create(at);
            return at;
        }
        return null;
    }

    @Override
    public void remove(String accessToken) {
        redisTemplate.delete(TOKEN_KEY + accessToken);
    }
}
