package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.server.entity.RefreshTokenContent;
import com.smart.sso.server.token.RefreshTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地刷新凭证管理
 * 
 * @author Joe
 */
public class LocalRefreshTokenManager implements RefreshTokenManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

    private int timeout;

	private Map<String, ObjectWrapper<RefreshTokenContent>> refreshTokenMap = new ConcurrentHashMap<>();

	public LocalRefreshTokenManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void create(String refreshToken, RefreshTokenContent refreshTokenContent) {
		ObjectWrapper<RefreshTokenContent> wrapper = new ObjectWrapper<>(refreshTokenContent,
				System.currentTimeMillis() + getExpiresIn() * 1000);
		refreshTokenMap.put(refreshToken, wrapper);
	}

	@Override
	public RefreshTokenContent validate(String rt) {
		ObjectWrapper<RefreshTokenContent> wrapper = refreshTokenMap.remove(rt);
		if (wrapper == null || System.currentTimeMillis() > wrapper.getExpired()) {
			return null;
		}
		return wrapper.getObject();
	}

	@Override
	public void verifyExpired() {
		refreshTokenMap.forEach((resfreshToken, wrapper) -> {
			if (System.currentTimeMillis() > wrapper.getExpired()) {
				refreshTokenMap.remove(resfreshToken);
				logger.debug("resfreshToken : " + resfreshToken + "已失效");
			}
		});
	}
	
	/*
	 * refreshToken时效和自定义的登录超时时效保持一致
	 */
	@Override
	public int getExpiresIn() {
		return timeout;
	}
}
