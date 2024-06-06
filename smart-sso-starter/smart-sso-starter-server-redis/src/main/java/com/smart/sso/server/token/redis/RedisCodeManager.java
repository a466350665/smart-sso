package com.smart.sso.server.token.redis;

import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.token.CodeManager;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式授权码管理
 * 
 * @author Joe
 */
public class RedisCodeManager implements CodeManager {

	private StringRedisTemplate redisTemplate;

	public RedisCodeManager(StringRedisTemplate redisTemplate) {
		this.redisTemplate = redisTemplate;
	}

	@Override
	public void create(String code, CodeContent codeContent) {
		redisTemplate.opsForValue().set(code, JsonUtils.toJSONString(codeContent), getExpiresIn(), TimeUnit.SECONDS);
	}

	@Override
	public CodeContent getAndRemove(String code) {
		String cc = redisTemplate.opsForValue().get(code);
		if (!StringUtils.isEmpty(cc)) {
			redisTemplate.delete(code);
		}
		return JsonUtils.parseObject(cc, CodeContent.class);
	}
}
