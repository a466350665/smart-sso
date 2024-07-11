package openjoe.smart.sso.server.manager.redis;

import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.server.entity.CodeContent;
import openjoe.smart.sso.server.manager.AbstractCodeManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式授权码管理
 *
 * @author Joe
 */
public class RedisCodeManager extends AbstractCodeManager {

    protected final Logger logger = LoggerFactory.getLogger(RedisCodeManager.class);
    private static final String CODE_KEY = "server_code_";

    private StringRedisTemplate redisTemplate;

    public RedisCodeManager(int timeout, StringRedisTemplate redisTemplate) {
        super(timeout);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String code, CodeContent codeContent) {
        redisTemplate.opsForValue().set(CODE_KEY + code, JsonUtils.toString(codeContent), getTimeout(), TimeUnit.SECONDS);
        logger.debug("Redis授权码创建成功, code:{}", code);
    }

    @Override
    public CodeContent get(String code) {
        String cc = redisTemplate.opsForValue().get(CODE_KEY + code);
        if (!StringUtils.hasLength(cc)) {
            return null;
        }
        return JsonUtils.parseObject(cc, CodeContent.class);
    }

    @Override
    public void remove(String code) {
        redisTemplate.delete(CODE_KEY + code);
    }
}
