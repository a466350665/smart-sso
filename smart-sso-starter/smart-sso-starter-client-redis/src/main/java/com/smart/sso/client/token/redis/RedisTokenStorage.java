package com.smart.sso.client.token.redis;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import com.smart.sso.client.util.Oauth2Utils;
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

    private ClientProperties properties;
    private StringRedisTemplate redisTemplate;

    public RedisTokenStorage(ClientProperties properties, StringRedisTemplate redisTemplate) {
        this.properties = properties;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String st, AccessToken at) {
        TokenWrapper wrapper = new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn());
        redisTemplate.opsForValue().set(ST_TOKEN_KEY + st, JsonUtils.toJSONString(wrapper), at.getRefreshExpiresIn(),
                TimeUnit.SECONDS);
        redisTemplate.opsForValue().set(TOKEN_ST_KEY + at.getAccessToken(), st, at.getRefreshExpiresIn(),
                TimeUnit.SECONDS);
    }

    @Override
    public AccessToken get(String st) {
        String str = redisTemplate.opsForValue().get(ST_TOKEN_KEY + st);
        if (StringUtils.isEmpty(str)) {
            return null;
        }
        TokenWrapper wrapper = JsonUtils.parseObject(str, TokenWrapper.class);
        // accessToken没过期直接返回
        if (!wrapper.checkExpired()) {
            return wrapper.getObject();
        }
        return null;
    }

    @Override
    public AccessToken getAndRefresh(String st) {
        String str = redisTemplate.opsForValue().get(ST_TOKEN_KEY + st);
        if (StringUtils.isEmpty(str)) {
            return null;
        }
        TokenWrapper wrapper = JsonUtils.parseObject(str, TokenWrapper.class);
        // accessToken没过期直接返回
        if (wrapper != null && !wrapper.checkExpired()) {
            return wrapper.getObject();
        }

        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        AccessToken at = Oauth2Utils.getRefreshToken(properties, wrapper.getObject().getRefreshToken());
        if (at != null) {
            remove(st);
            create(st, at);
            return at;
        }
        return null;
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
