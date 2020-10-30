package com.smart.sso.server.session.local;

import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.CollectionUtils;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.util.HttpUtils;
import com.smart.sso.server.common.AccessTokenContent;
import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.common.TimeoutParamter;
import com.smart.sso.server.session.AccessTokenManager;

/**
 * 本地AccessToken管理
 * 
 * @author Joe
 */
public class LocalAccessTokenManager extends TimeoutParamter implements AccessTokenManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private final Map<String, DummyAccessToken> accessTokenMap = new ConcurrentHashMap<>();
	private final Map<String, Set<String>> tgtMap = new ConcurrentHashMap<>();

	public LocalAccessTokenManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void generate(String accessToken, String service, String tgt) {
		AccessTokenContent accessTokenContent = new AccessTokenContent(service, tgt);
		DummyAccessToken dat = new DummyAccessToken(accessTokenContent, System.currentTimeMillis() + timeout * 1000);
		accessTokenMap.put(accessToken, dat);

		Set<String> accessTokenSet = tgtMap.get(tgt);
		if (accessTokenSet == null) {
			accessTokenSet = new HashSet<>();
			tgtMap.put(tgt, accessTokenSet);
		}
		accessTokenSet.add(accessToken);
	}
	
	@Override
	public AccessTokenContent validate(String accessToken) {
		DummyAccessToken dummyRt = accessTokenMap.get(accessToken);
		if (dummyRt == null || System.currentTimeMillis() > dummyRt.expired) {
			return null;
		}
		return dummyRt.accessTokenContent;
	}
	
	@Override
	public String refresh(String accessToken) {
		DummyAccessToken dummyRt = accessTokenMap.get(accessToken);
		if (dummyRt == null || System.currentTimeMillis() > dummyRt.expired) {
			return null;
		}
		dummyRt.expired = System.currentTimeMillis() + timeout * 1000;
		return accessToken;
	}

	@Override
	public void remove(String tgt) {
		Set<String> accessTokenSet = tgtMap.remove(tgt);
		if (CollectionUtils.isEmpty(accessTokenSet)) {
			return;
		}
		accessTokenSet.forEach(accessToken -> {
			DummyAccessToken dummyRt = accessTokenMap.get(accessToken);
			if (dummyRt == null || System.currentTimeMillis() > dummyRt.expired) {
				return;
			}
			HttpUtils.get(dummyRt.accessTokenContent.getService() + "?" + SsoConstant.LOGOUT_PARAMETER_NAME + "=" + accessToken);
		});
	}

	@Scheduled(cron = "0 */1 * * * ?")
	@Override
	public void verifyExpired() {
		for (Entry<String, DummyAccessToken> entry : accessTokenMap.entrySet()) {
			String resfreshToken = entry.getKey();
			DummyAccessToken dummyRt = entry.getValue();
			// 已过期
			if (System.currentTimeMillis() > dummyRt.expired) {
				accessTokenMap.remove(resfreshToken);
				logger.debug("resfreshToken : " + resfreshToken + "已失效");
			}
		}
	}

	private class DummyAccessToken {
		private AccessTokenContent accessTokenContent;
		private long expired; // 过期时间

		public DummyAccessToken(AccessTokenContent accessTokenContent, long expired) {
			super();
			this.accessTokenContent = accessTokenContent;
			this.expired = expired;
		}
	}

	@Override
	public int getExpiresIn() {
		return timeout;
	}
}
