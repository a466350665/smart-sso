package com.smart.sso.server.token;

import com.smart.sso.base.constant.BaseConstant;
import com.smart.sso.base.entity.Expiration;
import com.smart.sso.base.entity.LifecycleManager;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.HttpUtils;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.entity.TokenContent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * 调用凭证AccessToken管理抽象
 * 
 * @author Joe
 */
public abstract class TokenManager implements LifecycleManager<TokenContent>, Expiration {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected int timeout;

	public TokenManager(int timeout) {
		this.timeout = timeout;
	}

	/**
	 * 通过TGT移除
	 *
	 * @param tgt
	 */
	public abstract void removeByTgt(String tgt);

	/**
	 * 生成AccessToken
	 *
	 * @param codeContent
	 * @param userinfo
	 * @param appId
	 * @return
	 */
	public TokenContent generate(CodeContent codeContent, Userinfo userinfo, String appId) {
		String accessToken = "AT-" + UUID.randomUUID().toString().replaceAll("-", "");
		String refreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
		TokenContent tc = new TokenContent(accessToken, refreshToken, codeContent, userinfo, appId);
		create(refreshToken, tc);
		return tc;
	}
	
	/**
	 * 发起客户端退出请求
	 * 
	 * @param redirectUri
	 * @param accessToken
	 */
	protected void sendLogoutRequest(String redirectUri, String accessToken) {
		Map<String, String> headerMap = new HashMap<>();
		headerMap.put(BaseConstant.LOGOUT_PARAMETER_NAME, accessToken);
		HttpUtils.postHeader(redirectUri, headerMap);
	}

	/**
	 * refreshToken时效和自定义的登录超时时效保持一致
	 */
	public int getRefreshExpiresIn() {
		return 2 * timeout;
	}

	/**
	 * accessToken时效为登录超时时效的1/2
	 */
	@Override
	public int getExpiresIn() {
		return timeout;
	}
}
