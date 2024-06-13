package com.smart.sso.server.token.redis;

import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.token.AbstractTicketGrantingTicketManager;
import com.smart.sso.server.token.AbstractTokenManager;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式登录凭证管理
 *
 * @author Joe
 */
public class RedisTicketGrantingTicketManager extends AbstractTicketGrantingTicketManager {

    private static final String TGT_KEY = "server_tgt_";
    private StringRedisTemplate redisTemplate;

    public RedisTicketGrantingTicketManager(AbstractTokenManager tokenManager, int timeout, StringRedisTemplate redisTemplate) {
        super(tokenManager, timeout);
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void create(String tgt, Userinfo userinfo) {
        redisTemplate.opsForValue().set(TGT_KEY + tgt, JsonUtils.toString(userinfo), getExpiresIn(),
                TimeUnit.SECONDS);
        logger.debug("Redis登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public Userinfo get(String tgt) {
        String userinfo = redisTemplate.opsForValue().get(TGT_KEY + tgt);
        if (!StringUtils.hasLength(userinfo)) {
            return null;
        }
        redisTemplate.expire(TGT_KEY + tgt, getExpiresIn(), TimeUnit.SECONDS);
        return JsonUtils.parseObject(userinfo, Userinfo.class);
    }

    @Override
    public void remove(String tgt) {
        redisTemplate.delete(TGT_KEY + tgt);
        logger.debug("Redis登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void refresh(String tgt) {
        redisTemplate.expire(TGT_KEY + tgt, getExpiresIn(), TimeUnit.SECONDS);
    }
}