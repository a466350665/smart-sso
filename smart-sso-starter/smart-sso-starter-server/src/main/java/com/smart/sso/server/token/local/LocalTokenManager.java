package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.entity.TokenContent;
import com.smart.sso.server.token.TokenManager;
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
public class LocalTokenManager extends TokenManager implements ExpirationPolicy {

	private Map<String, ObjectWrapper<TokenContent>> tokenMap = new ConcurrentHashMap<>();
	private Map<String, Set<String>> tgtMap = new ConcurrentHashMap<>();

	public LocalTokenManager(int timeout) {
		super(timeout);
	}

	@Override
	public void create(String refreshToken, TokenContent tokenContent) {
		ObjectWrapper<TokenContent> dat = new ObjectWrapper(tokenContent, getRefreshExpiresIn());
		tokenMap.put(refreshToken, dat);

		tgtMap.computeIfAbsent(tokenContent.getCodeContent().getTgt(), a -> new HashSet<>()).add(refreshToken);
		logger.info("调用凭证生成成功, accessToken:{}, refreshToken:{}", tokenContent.getAccessToken(), refreshToken);
	}
	
	@Override
	public TokenContent get(String refreshToken) {
		ObjectWrapper<TokenContent> wrapper = tokenMap.get(refreshToken);
		if (wrapper == null || wrapper.checkExpired()) {
			return null;
		}
		else {
			return wrapper.getObject();
		}
	}

	@Override
	public void remove(String refreshToken) {
		ObjectWrapper<TokenContent> wrapper = tokenMap.remove(refreshToken);
		if(wrapper == null){
			return;
		}
		Set<String> tokenSet = tgtMap.get(wrapper.getObject().getCodeContent().getTgt());
		if (CollectionUtils.isEmpty(tokenSet)) {
			return;
		}
		tokenSet.remove(refreshToken);
	}

	@Override
	public void removeByTgt(String tgt) {
		// 删除所有tgt对应的调用凭证
		Set<String> tokenSet = tgtMap.remove(tgt);
		if (CollectionUtils.isEmpty(tokenSet)) {
			return;
		}
		// 通知所有客户端退出，并注销本地Token
		tokenSet.forEach(refreshToken -> {
			ObjectWrapper<TokenContent> wrapper = tokenMap.remove(refreshToken);
			if (wrapper == null) {
				return;
			}
			CodeContent codeContent = wrapper.getObject().getCodeContent();
			if (codeContent == null) {
				return;
			}
			logger.info("发起客户端退出请求, accessToken:{}, refreshToken:{}, url:{}", wrapper.getObject().getAccessToken(), refreshToken, codeContent.getRedirectUri());
			sendLogoutRequest(codeContent.getRedirectUri(), wrapper.getObject().getAccessToken());
		});
	}

	@Override
	public void verifyExpired() {
		tokenMap.forEach((refreshToken, wrapper) -> {
			if (wrapper.checkExpired()) {
				remove(refreshToken);
				logger.info("调用凭证已失效, accessToken:{}, refreshToken:{}", wrapper.getObject().getAccessToken(), refreshToken);
			}
		});
	}
}
