package com.smart.sso.server.common;

import javax.annotation.Resource;

import com.smart.mvc.cache.RedisCache;

/**
 * 分布式环境令牌管理
 * 
 * @author Joe
 */
public class RedisTokenManager extends TokenManager {

	/**
	 * 是否需要扩展token过期时间
	 */
	private volatile boolean isNeedExtendExpired = false;

	@Resource
	private RedisCache<LoginUser> redisCache;

	@Override
	public void addToken(String token, LoginUser loginUser) {
		redisCache.set(token, loginUser, tokenTimeout * 1000);
	}

	@Override
	public LoginUser validate(String token) {
		LoginUser loginUser = redisCache.get(token);
		if (loginUser != null && isNeedExtendExpired) {
			isNeedExtendExpired = false;
			addToken(token, loginUser);
		}
		return loginUser;
	}

	@Override
	public void remove(String token) {
		redisCache.delete(token);
	}

	@Override
	public void verifyExpired() {
		isNeedExtendExpired = true;
	}
}
