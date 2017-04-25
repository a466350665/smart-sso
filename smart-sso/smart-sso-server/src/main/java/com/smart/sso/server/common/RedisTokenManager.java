package com.smart.sso.server.common;

import javax.annotation.Resource;

import com.smart.mvc.cache.RedisCache;

/**
 * 分布式环境令牌管理
 * 
 * @author Joe
 */
public class RedisTokenManager extends TokenManager {

	@Resource
	private RedisCache<LoginUser> redisCache;

	@Override
	public void addToken(String token, LoginUser loginUser) {
		redisCache.set(token, loginUser, tokenTimeout);
	}

	@Override
	public LoginUser validate(String token) {
		return redisCache.get(token);
	}

	@Override
	public void remove(String token) {
		redisCache.delete(token);
	}
}
