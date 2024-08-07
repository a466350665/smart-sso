package openjoe.smart.sso.server.manager.redis;

import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式登录凭证管理
 *
 * @author Joe
 */
public class RedisTicketGrantingTicketManager extends AbstractTicketGrantingTicketManager {

    protected final Logger logger = LoggerFactory.getLogger(RedisTicketGrantingTicketManager.class);
    private static final String TGT_KEY = "server_tgt_";
    private StringRedisTemplate redisTemplate;

    public RedisTicketGrantingTicketManager(int timeout, String cookieName, AbstractTokenManager tokenManager, StringRedisTemplate redisTemplate) {
        super(timeout, cookieName, tokenManager);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String tgt, TokenUser tokenUser) {
        redisTemplate.opsForValue().set(TGT_KEY + tgt, JsonUtils.toString(tokenUser), getTimeout(),
                TimeUnit.SECONDS);
        logger.debug("Redis登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public TokenUser get(String tgt) {
        String tokenUser = redisTemplate.opsForValue().get(TGT_KEY + tgt);
        if (!StringUtils.hasLength(tokenUser)) {
            return null;
        }
        redisTemplate.expire(TGT_KEY + tgt, getTimeout(), TimeUnit.SECONDS);
        return JsonUtils.parseObject(tokenUser, TokenUser.class);
    }

    @Override
    public void remove(String tgt) {
        redisTemplate.delete(TGT_KEY + tgt);
        logger.debug("Redis登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void refresh(String tgt) {
        redisTemplate.expire(TGT_KEY + tgt, getTimeout(), TimeUnit.SECONDS);
    }
}