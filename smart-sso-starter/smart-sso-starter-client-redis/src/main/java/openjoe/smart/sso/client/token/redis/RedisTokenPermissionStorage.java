package openjoe.smart.sso.client.token.redis;

import com.fasterxml.jackson.core.type.TypeReference;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.client.token.TokenPermissionStorage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 凭证权限信息存储Redis实现
 *
 * @author Joe
 */
public final class RedisTokenPermissionStorage implements TokenPermissionStorage {
    private final Logger logger = LoggerFactory.getLogger(RedisTokenPermissionStorage.class);
    private static final String TOKEN_PERMISSION_KEY = "client_tp_";

    private StringRedisTemplate redisTemplate;

    public RedisTokenPermissionStorage(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String accessToken, ExpirationWrapper<TokenPermission> wrapper) {
        redisTemplate.opsForValue().set(TOKEN_PERMISSION_KEY + accessToken, JsonUtils.toString(wrapper.getObject()), wrapper.getExpiresIn(),
                TimeUnit.SECONDS);
        logger.debug("Redis凭证权限信息创建成功, accessToken:{}", accessToken);
    }

    @Override
    public ExpirationWrapper<TokenPermission> get(String accessToken) {
        String str = redisTemplate.opsForValue().get(TOKEN_PERMISSION_KEY + accessToken);
        if (!StringUtils.hasLength(str)) {
            return null;
        }
        return JsonUtils.parseObject(str, new TypeReference<ExpirationWrapper<TokenPermission>>() {});
    }

    @Override
    public void remove(String accessToken) {
        redisTemplate.delete(TOKEN_PERMISSION_KEY + accessToken);
    }
}
