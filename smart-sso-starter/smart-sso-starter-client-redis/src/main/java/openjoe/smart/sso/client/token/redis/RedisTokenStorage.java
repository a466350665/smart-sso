package openjoe.smart.sso.client.token.redis;

import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.TokenWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * Token存储Redis实现
 *
 * @author Joe
 */
public final class RedisTokenStorage implements TokenStorage {
    private final Logger logger = LoggerFactory.getLogger(RedisTokenStorage.class);
    private static final String ACCESS_TOKEN_KEY = "client_at_";
    private static final String REFRESH_TOKEN_KEY = "client_rt_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenStorage(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String accessToken, TokenWrapper wrapper) {
        redisTemplate.opsForValue().set(ACCESS_TOKEN_KEY + accessToken, JsonUtils.toString(wrapper), wrapper.getObject().getRefreshExpiresIn(),
                TimeUnit.SECONDS);

        redisTemplate.opsForValue().set(REFRESH_TOKEN_KEY + wrapper.getObject().getRefreshToken(), accessToken, wrapper.getObject().getRefreshExpiresIn(),
                TimeUnit.SECONDS);
        logger.debug("Redis服务凭证创建成功, accessToken:{}", accessToken);
    }

    @Override
    public TokenWrapper get(String accessToken) {
        String str = redisTemplate.opsForValue().get(ACCESS_TOKEN_KEY + accessToken);
        if (!StringUtils.hasLength(str)) {
            return null;
        }
        return JsonUtils.parseObject(str, TokenWrapper.class);
    }

    @Override
    public void remove(String accessToken) {
        TokenWrapper wrapper = get(accessToken);
        if (wrapper == null) {
            return;
        }
        redisTemplate.delete(ACCESS_TOKEN_KEY + accessToken);
        redisTemplate.delete(REFRESH_TOKEN_KEY + wrapper.getObject().getRefreshToken());
    }

    @Override
    public String getAccessToken(String refreshToken) {
        return redisTemplate.opsForValue().get(REFRESH_TOKEN_KEY + refreshToken);
    }
}
