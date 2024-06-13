package com.smart.sso.server.token.redis;

import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.entity.TicketGrantingTicketContent;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import com.smart.sso.server.token.TokenManager;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式登录凭证管理
 * 
 * @author Joe
 */
public class RedisTicketGrantingTicketManager extends TicketGrantingTicketManager {

	private static final String TGT_KEY = "server_tgt_";
	private StringRedisTemplate redisTemplate;

	public RedisTicketGrantingTicketManager(TokenManager tokenManager, int timeout, StringRedisTemplate redisTemplate) {
		super(tokenManager, timeout);
		this.redisTemplate = redisTemplate;
	}

	@Override
	public void create(String tgt, TicketGrantingTicketContent tgtContent) {
		redisTemplate.opsForValue().set(TGT_KEY + tgt, JsonUtils.toJSONString(tgtContent), getExpiresIn(),
				TimeUnit.SECONDS);
		logger.info("Redis登录凭证生成成功, tgt:{}", tgt);
	}

	@Override
	public TicketGrantingTicketContent get(String tgt) {
		String tgtContent = redisTemplate.opsForValue().get(TGT_KEY + tgt);
		if (StringUtils.isEmpty(tgtContent)) {
			return null;
		}
		redisTemplate.expire(TGT_KEY + tgt, getExpiresIn(), TimeUnit.SECONDS);
		return JsonUtils.parseObject(tgtContent, TicketGrantingTicketContent.class);
	}

	@Override
	public void remove(String tgt) {
		redisTemplate.delete(TGT_KEY + tgt);
		logger.info("Redis登录凭证删除成功, tgt:{}", tgt);
	}

	@Override
	public void refresh(String tgt) {
		redisTemplate.expire(TGT_KEY + tgt, getExpiresIn(), TimeUnit.SECONDS);
	}
}