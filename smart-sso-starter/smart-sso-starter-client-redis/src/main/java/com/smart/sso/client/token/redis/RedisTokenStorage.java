package com.smart.sso.client.token.redis;

import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * Token存储Redis实现
 *
 * @author Joe
 */
public final class RedisTokenStorage extends TokenStorage {

    private static final String ST_TOKEN_KEY = "st_token_key_";
    private static final String TOKEN_ST_KEY = "token_st_key_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenStorage(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String st, TokenWrapper wrapper) {
        redisTemplate.opsForValue().set(ST_TOKEN_KEY + st, JsonUtils.toJSONString(wrapper), wrapper.getObject().getRefreshExpiresIn(),
                TimeUnit.SECONDS);
        redisTemplate.opsForValue().set(TOKEN_ST_KEY + wrapper.getObject().getAccessToken(), st, wrapper.getObject().getRefreshExpiresIn(),
                TimeUnit.SECONDS);
    }

    @Override
    public TokenWrapper get(String st) {
        String str = redisTemplate.opsForValue().get(ST_TOKEN_KEY + st);
        if (StringUtils.isEmpty(str)) {
            return null;
        }
        return JsonUtils.parseObject(str, TokenWrapper.class);
    }

    @Override
    public void remove(String st) {
        String str = redisTemplate.opsForValue().get(ST_TOKEN_KEY + st);
        if (StringUtils.isEmpty(str)) {
            return;
        }
        redisTemplate.delete(ST_TOKEN_KEY + st);
        TokenWrapper wrapper = JsonUtils.parseObject(str, TokenWrapper.class);
        if (wrapper != null) {
            redisTemplate.delete(TOKEN_ST_KEY + wrapper.getObject().getAccessToken());
        }
    }

    @Override
    public void removeByAccessToken(String accessToken) {
        String st = redisTemplate.opsForValue().get(TOKEN_ST_KEY + accessToken);
        redisTemplate.delete(TOKEN_ST_KEY + accessToken);
        redisTemplate.delete(ST_TOKEN_KEY + st);
    }
}
