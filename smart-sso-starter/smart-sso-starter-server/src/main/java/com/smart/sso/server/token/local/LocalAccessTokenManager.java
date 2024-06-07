package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.server.entity.AccessTokenContent;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.token.AccessTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

	private Map<String, ObjectWrapper<AccessTokenContent>> accessTokenMap = new ConcurrentHashMap<>();
	private Map<String, Set<String>> tgtMap = new ConcurrentHashMap<>();

	public LocalAccessTokenManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void create(String accessToken, AccessTokenContent accessTokenContent) {
		ObjectWrapper<AccessTokenContent> dat = new ObjectWrapper(accessTokenContent, System.currentTimeMillis() + getExpiresIn() * 1000);
		accessTokenMap.put(accessToken, dat);

		tgtMap.computeIfAbsent(accessTokenContent.getCodeContent().getTgt(), a -> new HashSet<>()).add(accessToken);
		logger.info("调用凭证生成成功, accessToken:{}", accessToken);
	}
	
	@Override
	public AccessTokenContent get(String accessToken) {
		ObjectWrapper<AccessTokenContent> wrapper = accessTokenMap.get(accessToken);
		if (wrapper == null || System.currentTimeMillis() > wrapper.getExpired()) {
			return null;
		}
		else {
			return wrapper.getObject();
		}
	}
	
	@Override
	public boolean refresh(String accessToken) {
		ObjectWrapper<AccessTokenContent> wrapper = accessTokenMap.get(accessToken);
		if (wrapper == null || System.currentTimeMillis() > wrapper.getExpired()) {
			return false;
		}
		wrapper.setExpired(System.currentTimeMillis() + getExpiresIn() * 1000);
		return true;
	}

	@Override
	public void remove(String tgt) {
		Set<String> accessTokenSet = tgtMap.remove(tgt);
		if (CollectionUtils.isEmpty(accessTokenSet)) {
			return;
		}
		accessTokenSet.forEach(accessToken -> {
			ObjectWrapper<AccessTokenContent> wrapper = accessTokenMap.get(accessToken);
			if (wrapper == null || System.currentTimeMillis() > wrapper.getExpired()) {
				return;
			}
			CodeContent codeContent = wrapper.getObject().getCodeContent();
			if (codeContent == null || !codeContent.isSendLogoutRequest()) {
				return;
			}
			logger.debug("发起客户端退出请求, accessToken:{}, url:{}", accessToken, codeContent.getRedirectUri());
			sendLogoutRequest(codeContent.getRedirectUri(), accessToken);
		});
	}

	@Override
	public void verifyExpired() {
		accessTokenMap.forEach((accessToken, wrapper) -> {
			if (System.currentTimeMillis() > wrapper.getExpired()) {
				accessTokenMap.remove(accessToken);
				logger.debug("调用凭证已失效, accessToken:{}", accessToken);
			}
		});
	}
	
	/**
	 * accessToken时效为登录超时时效的1/2
	 */
	@Override
	public int getExpiresIn() {
		return timeout / 2;
	}
}
