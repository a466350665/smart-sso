package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.session.RefreshTokenManager;

/**
 * 本地刷新凭证管理
 * 
 * @author Joe
 */
@Component
@ConditionalOnProperty(name = "sso.session.manager", havingValue = "local")
public class LocalRefreshTokenManager implements RefreshTokenManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());
	
	@Value("${sso.timeout}")
    private int timeout;

	private Map<String, DummyRefreshToken> refreshTokenMap = new ConcurrentHashMap<>();

	@Override
	public void create(String refreshToken, String accessToken, String appId, String service, String tgt) {
		RefreshTokenContent refreshTokenContent = new RefreshTokenContent(service, tgt, accessToken, appId);
		DummyRefreshToken dummyRt = new DummyRefreshToken(refreshTokenContent,
				System.currentTimeMillis() + getExpiresIn() * 1000);
		refreshTokenMap.put(refreshToken, dummyRt);
	}

	@Override
	public RefreshTokenContent validate(String rt) {
		DummyRefreshToken dummyRt = refreshTokenMap.remove(rt);
		if (dummyRt == null || System.currentTimeMillis() > dummyRt.expired) {
			return null;
		}
		return dummyRt.refreshTokenContent;
	}

	@Scheduled(cron = "0 */1 * * * ?")
	@Override
	public void verifyExpired() {
		refreshTokenMap.forEach((resfreshToken, dummyRt) -> {
			if (System.currentTimeMillis() > dummyRt.expired) {
				refreshTokenMap.remove(resfreshToken);
				logger.debug("resfreshToken : " + resfreshToken + "已失效");
			}
		});
	}
	
	/*
	 * refreshToken时效和登录session时效一致
	 */
	@Override
	public int getExpiresIn() {
		return timeout;
	}

	private class DummyRefreshToken {
		private RefreshTokenContent refreshTokenContent;
		private long expired; // 过期时间

		public DummyRefreshToken(RefreshTokenContent refreshTokenContent, long expired) {
			super();
			this.refreshTokenContent = refreshTokenContent;
			this.expired = expired;
		}
	}
}
