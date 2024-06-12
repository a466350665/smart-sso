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

	private StringRedisTemplate redisTemplate;

	public RedisTicketGrantingTicketManager(TokenManager tokenManager, int timeout, StringRedisTemplate redisTemplate) {
		super(tokenManager, timeout);
		this.redisTemplate = redisTemplate;
	}

	@Override
	public void create(String tgt, TicketGrantingTicketContent tgtContent) {
		redisTemplate.opsForValue().set(tgt, JsonUtils.toJSONString(tgtContent), getExpiresIn(),
				TimeUnit.SECONDS);
	}

	@Override
	public TicketGrantingTicketContent get(String tgt) {
		String tgtContent = redisTemplate.opsForValue().get(tgt);
		if (StringUtils.isEmpty(tgtContent)) {
			return null;
		}
		redisTemplate.expire(tgt, getExpiresIn(), TimeUnit.SECONDS);
		return JsonUtils.parseObject(tgtContent, TicketGrantingTicketContent.class);
	}

	@Override
	public void remove(String tgt) {
		redisTemplate.delete(tgt);
	}

	@Override
	public void refresh(String tgt) {
		redisTemplate.expire(tgt, getExpiresIn(), TimeUnit.SECONDS);
	}

//	@Override
//	public void verifyExpired() {
//		// TODO 监听TGT过期，并通知删除所有Token
//		// removeTgtAndToken();
//	}
}