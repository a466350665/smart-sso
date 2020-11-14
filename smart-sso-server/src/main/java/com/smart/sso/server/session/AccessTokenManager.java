package com.smart.sso.server.session;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.util.HttpUtils;
import com.smart.sso.server.common.AccessTokenContent;
import com.smart.sso.server.common.Expiration;
import com.smart.sso.server.enums.ClientTypeEnum;

/**
 * 调用凭证AccessToken管理抽象
 * 
 * @author Joe
 */
public interface AccessTokenManager extends Expiration {

	/**
	 * 生成AccessToken
	 * 
	 * @param tgt
	 * @param clientType
	 * @param redirectUri
	 * @return
	 */
	default String generate(String tgt, ClientTypeEnum clientType, String redirectUri) {
		String accessToken = "AT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(accessToken, new AccessTokenContent(tgt, clientType, redirectUri));
		return accessToken;
	}

	/**
	 * 生成AccessToken
	 * 
	 * @param accessToken
	 * @param accessTokenContent
	 */
	void create(String accessToken, AccessTokenContent accessTokenContent);
	
	/**
	 * 验证accessToken有效性，无论有效性与否，都remove掉
	 * 
	 * @param accessToken
	 * @return
	 */
	AccessTokenContent validate(String accessToken);

	/**
     * 延长AccessToken生命周期
     * 
	 * @param accessToken
	 * @return
	 */
	boolean refresh(String accessToken);
	
	/**
	 * 根据TGT删除AccessToken
	 * 
	 * @param tgt
	 */
	void remove(String tgt);
	
	/**
	 * 发起客户端登出请求
	 * 
	 * @param redirectUri
	 * @param accessToken
	 */
	default void sendLogoutRequest(String redirectUri, String accessToken) {
		Map<String, String> headerMap = new HashMap<>();
		headerMap.put(SsoConstant.LOGOUT_PARAMETER_NAME, accessToken);
		HttpUtils.postHeader(redirectUri, headerMap);
	}
}
