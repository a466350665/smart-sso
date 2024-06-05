package com.smart.sso.server.session.redis;

import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.session.RefreshTokenManager;
import com.smart.sso.server.util.JsonUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.StringUtils;

import java.util.concurrent.TimeUnit;

/**
 * 分布式刷新凭证管理
 * 
 * @author Joe
 */
public class RedisRefreshTokenManager implements RefreshTokenManager {

	private StringRedisTemplate redisTemplate;
	private int timeout;

	public RedisRefreshTokenManager(StringRedisTemplate redisTemplate, int timeout) {
		this.redisTemplate = redisTemplate;
		this.timeout = timeout;
	}

	@Override
	public void create(String refreshToken, RefreshTokenContent refreshTokenContent) {
		redisTemplate.opsForValue().set(refreshToken, JsonUtils.toJSONString(refreshTokenContent), getExpiresIn(),
				TimeUnit.SECONDS);
	}

	@Override
	public RefreshTokenContent validate(String refreshToken) {
		String rtc = redisTemplate.opsForValue().get(refreshToken);
		if (!StringUtils.isEmpty(rtc)) {
			redisTemplate.delete(refreshToken);
		}
		return JsonUtils.parseObject(rtc, RefreshTokenContent.class);
	}
	
	/*
	 * refreshToken时效和登录session时效一致
	 */
	@Override
	public int getExpiresIn() {
		return timeout;
	}
}
