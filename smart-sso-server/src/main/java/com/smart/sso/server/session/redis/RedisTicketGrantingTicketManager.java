package com.smart.sso.server.session.redis;

import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.server.session.TicketGrantingTicketManager;

/**
 * 分布式登录凭证管理
 * 
 * @author Joe
 */
@Component
@ConditionalOnProperty(name = "sso.session.manager", havingValue = "redis")
public class RedisTicketGrantingTicketManager implements TicketGrantingTicketManager {
	
	@Value("${sso.timeout}")
    private int timeout;
	@Autowired
	private StringRedisTemplate redisTemplate;

	@Override
	public void create(String tgt, RpcUser user) {
		redisTemplate.opsForValue().set(tgt, JSON.toJSONString(user), getExpiresIn(),
				TimeUnit.SECONDS);
	}

	@Override
	public RpcUser exists(String tgt) {
		String user = redisTemplate.opsForValue().get(tgt);
		if (StringUtils.isEmpty(user)) {
			return null;
		}
		return JSONObject.parseObject(user, RpcUser.class);
	}

	@Override
	public void remove(String tgt) {
		redisTemplate.delete(tgt);
	}

	@Override
	public boolean refresh(String tgt) {
		if (redisTemplate.opsForValue().get(tgt) == null) {
			return false;
		}
		redisTemplate.expire(tgt, timeout, TimeUnit.SECONDS);
        return true;
	}
    
	@Override
	public int getExpiresIn() {
		return timeout;
	}
}