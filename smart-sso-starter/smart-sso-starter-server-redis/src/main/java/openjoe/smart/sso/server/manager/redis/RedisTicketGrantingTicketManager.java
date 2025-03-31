package openjoe.smart.sso.server.manager.redis;

import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.server.entity.TicketGrantingTicketContent;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;
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
    public void create(String tgt, TicketGrantingTicketContent tgtContent) {
        redisTemplate.opsForValue().set(TGT_KEY + tgt, JsonUtils.toString(tgtContent), getTimeout(),
                TimeUnit.SECONDS);
        logger.debug("Redis登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public TicketGrantingTicketContent get(String tgt) {
        String tgtContent = redisTemplate.opsForValue().get(TGT_KEY + tgt);
        if (!StringUtils.hasLength(tgtContent)) {
            return null;
        }
        redisTemplate.expire(TGT_KEY + tgt, getTimeout(), TimeUnit.SECONDS);
        return JsonUtils.parseObject(tgtContent, TicketGrantingTicketContent.class);
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

    @Override
    public Map<String, TicketGrantingTicketContent> getTgtMap(Set<Long> userIds, Long current, Long size) {
        Map<String, TicketGrantingTicketContent> resultMap = new LinkedHashMap<>();
        Set<String> keys = redisTemplate.keys(TGT_KEY + "*");
        if (CollectionUtils.isEmpty(keys)) {
            return resultMap;
        }

        List<String> sortedKeys = new ArrayList<>(keys);
        Collections.sort(sortedKeys);
        long start = (current - 1) * size;
        long end = start + size;
        long count = 0;

        for (String key : sortedKeys) {
            String tgt = key.replace(TGT_KEY, "");
            String tgtContentStr = redisTemplate.opsForValue().get(key);
            if (!StringUtils.hasLength(tgtContentStr)) {
                continue;
            }

            TicketGrantingTicketContent tgtContent = JsonUtils.parseObject(tgtContentStr, TicketGrantingTicketContent.class);
            if (CollectionUtils.isEmpty(userIds) || userIds.contains(tgtContent.getUserId())) {
                if (count >= start && count < end) {
                    resultMap.put(tgt, tgtContent);
                }
                count++;
                if (count >= end) {
                    break;
                }
            }
        }
        return resultMap;
    }
}