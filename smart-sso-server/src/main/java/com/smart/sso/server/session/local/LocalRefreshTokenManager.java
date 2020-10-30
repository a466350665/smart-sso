package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;

import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.common.TimeoutParamter;
import com.smart.sso.server.session.RefreshTokenManager;

/**
 * 本地RefreshToken管理
 * 
 * @author Joe
 */
public class LocalRefreshTokenManager extends TimeoutParamter implements RefreshTokenManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private final Map<String, DummyRefreshToken> refreshTokenMap = new ConcurrentHashMap<>();

	public LocalRefreshTokenManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void generate(String refreshToken, String accessToken, String appId, String service, String tgt) {
		RefreshTokenContent refreshTokenContent = new RefreshTokenContent(service, tgt, accessToken, appId);
		DummyRefreshToken dummyRt = new DummyRefreshToken(refreshTokenContent,
				System.currentTimeMillis() + timeout * 1000);
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
		for (Entry<String, DummyRefreshToken> entry : refreshTokenMap.entrySet()) {
			String resfreshToken = entry.getKey();
			DummyRefreshToken dummyRt = entry.getValue();
			// 已过期
			if (System.currentTimeMillis() > dummyRt.expired) {
				refreshTokenMap.remove(resfreshToken);
				logger.debug("resfreshToken : " + resfreshToken + "已失效");
			}
		}
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
