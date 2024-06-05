package com.smart.sso.server.session.redis;

import com.smart.sso.server.common.ServerUser;
import com.smart.sso.server.session.TicketGrantingTicketManager;
import com.smart.sso.server.util.JsonUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式登录凭证管理
 * 
 * @author Joe
 */
public class RedisTicketGrantingTicketManager implements TicketGrantingTicketManager {

	private StringRedisTemplate redisTemplate;
	private int timeout;

	public RedisTicketGrantingTicketManager(StringRedisTemplate redisTemplate, int timeout) {
		this.redisTemplate = redisTemplate;
		this.timeout = timeout;
	}

	@Override
	public void create(String tgt, ServerUser user) {
		redisTemplate.opsForValue().set(tgt, JsonUtils.toJSONString(user), getExpiresIn(),
				TimeUnit.SECONDS);
	}

	@Override
	public ServerUser getAndRefresh(String tgt) {
		String user = redisTemplate.opsForValue().get(tgt);
		if (StringUtils.isEmpty(user)) {
			return null;
		}
		redisTemplate.expire(tgt, timeout, TimeUnit.SECONDS);
		return JsonUtils.parseObject(user, ServerUser.class);
	}
	
	@Override
	public void set(String tgt, ServerUser user) {
		create(tgt, user);
	}

	@Override
	public void remove(String tgt) {
		redisTemplate.delete(tgt);
	}

	@Override
	public int getExpiresIn() {
		return timeout;
	}
}