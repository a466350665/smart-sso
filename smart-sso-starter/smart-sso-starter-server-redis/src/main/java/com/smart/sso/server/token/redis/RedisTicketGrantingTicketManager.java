package com.smart.sso.server.token.redis;

import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.token.TicketGrantingTicketManager;
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
	public void create(String tgt, Userinfo userinfo) {
		redisTemplate.opsForValue().set(tgt, JsonUtils.toJSONString(userinfo), getExpiresIn(),
				TimeUnit.SECONDS);
	}

	@Override
	public Userinfo getAndRefresh(String tgt) {
		String user = redisTemplate.opsForValue().get(tgt);
		if (StringUtils.isEmpty(user)) {
			return null;
		}
		redisTemplate.expire(tgt, timeout, TimeUnit.SECONDS);
		return JsonUtils.parseObject(user, Userinfo.class);
	}
	
	@Override
	public void set(String tgt, Userinfo user) {
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