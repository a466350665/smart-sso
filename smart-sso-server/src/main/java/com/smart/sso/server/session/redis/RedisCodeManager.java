package com.smart.sso.server.session.redis;

import java.util.concurrent.TimeUnit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.session.CodeManager;

/**
 * 分布式授权码管理
 * 
 * @author Joe
 */
@Component
@ConditionalOnProperty(name = "sso.session.manager", havingValue = "redis")
public class RedisCodeManager implements CodeManager {

	@Autowired
	private StringRedisTemplate redisTemplate;

	@Override
	public void create(String code, CodeContent codeContent) {
		redisTemplate.opsForValue().set(code, JSON.toJSONString(codeContent), getExpiresIn(), TimeUnit.SECONDS);
	}

	@Override
	public CodeContent getAndRemove(String code) {
		String cc = redisTemplate.opsForValue().get(code);
		if (!StringUtils.isEmpty(cc)) {
			redisTemplate.delete(code);
		}
		return JSONObject.parseObject(cc, CodeContent.class);
	}
}
