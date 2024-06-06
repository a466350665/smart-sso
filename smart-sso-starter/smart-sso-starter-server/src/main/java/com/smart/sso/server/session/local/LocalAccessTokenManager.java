package com.smart.sso.server.session.local;

import com.smart.sso.server.common.AccessTokenContent;
import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.session.AccessTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.CollectionUtils;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地调用凭证管理
 * 
 * @author Joe
 */
public class LocalAccessTokenManager implements AccessTokenManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

    private int timeout;

	private Map<String, AccessTokenWrapper> accessTokenMap = new ConcurrentHashMap<>();
	private Map<String, Set<String>> tgtMap = new ConcurrentHashMap<>();

	public LocalAccessTokenManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void create(String accessToken, AccessTokenContent accessTokenContent) {
		AccessTokenWrapper dat = new AccessTokenWrapper(accessTokenContent, System.currentTimeMillis() + getExpiresIn() * 1000);
		accessTokenMap.put(accessToken, dat);

		tgtMap.computeIfAbsent(accessTokenContent.getCodeContent().getTgt(), a -> new HashSet<>()).add(accessToken);
		logger.info("调用凭证生成成功, accessToken:{}", accessToken);
	}
	
	@Override
	public AccessTokenContent get(String accessToken) {
		AccessTokenWrapper wrapper = accessTokenMap.get(accessToken);
		if (wrapper == null || System.currentTimeMillis() > wrapper.expired) {
			return null;
		}
		else {
			return wrapper.accessTokenContent;
		}
	}
	
	@Override
	public boolean refresh(String accessToken) {
		AccessTokenWrapper wrapper = accessTokenMap.get(accessToken);
		if (wrapper == null || System.currentTimeMillis() > wrapper.expired) {
			return false;
		}
		wrapper.expired = System.currentTimeMillis() + getExpiresIn() * 1000;
		return true;
	}

	@Override
	public void remove(String tgt) {
		Set<String> accessTokenSet = tgtMap.remove(tgt);
		if (CollectionUtils.isEmpty(accessTokenSet)) {
			return;
		}
		accessTokenSet.forEach(accessToken -> {
			AccessTokenWrapper wrapper = accessTokenMap.get(accessToken);
			if (wrapper == null || System.currentTimeMillis() > wrapper.expired) {
				return;
			}
			CodeContent codeContent = wrapper.accessTokenContent.getCodeContent();
			if (codeContent == null || !codeContent.isSendLogoutRequest()) {
				return;
			}
			logger.debug("发起客户端登出请求, accessToken:{}, url:{}", accessToken, codeContent.getRedirectUri());
			sendLogoutRequest(codeContent.getRedirectUri(), accessToken);
		});
	}

	@Scheduled(cron = SCHEDULED_CRON)
	@Override
	public void verifyExpired() {
		accessTokenMap.forEach((accessToken, wrapper) -> {
			if (System.currentTimeMillis() > wrapper.expired) {
				accessTokenMap.remove(accessToken);
				logger.debug("调用凭证已失效, accessToken:{}", accessToken);
			}
		});
	}
	
	/**
	 * accessToken时效为登录session时效的1/2
	 */
	@Override
	public int getExpiresIn() {
		return timeout / 2;
	}

	private class AccessTokenWrapper {
		private AccessTokenContent accessTokenContent;
		private long expired; // 过期时间

		public AccessTokenWrapper(AccessTokenContent accessTokenContent, long expired) {
			super();
			this.accessTokenContent = accessTokenContent;
			this.expired = expired;
		}
	}
}
