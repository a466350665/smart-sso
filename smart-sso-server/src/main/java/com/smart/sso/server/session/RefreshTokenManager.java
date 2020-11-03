package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.Expiration;
import com.smart.sso.server.common.RefreshTokenContent;

/**
 * 刷新凭证refreshToken管理抽象
 * 
 * @author Joe
 */
public interface RefreshTokenManager extends Expiration {

	/**
	 * 生成refreshToken
	 * 
	 * @param accessToken
	 * @param service
	 * @param tgt
	 * @return
	 */
	default String generate(String accessToken, String appId, String service, String tgt) {
		String resfreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(resfreshToken, accessToken, appId, service, tgt);
		return resfreshToken;
	}

	/**
	 * 生成refreshToken
	 * 
	 * @param refreshToken
	 * @param accessToken
	 * @param service
	 * @param tgt
	 */
	void create(String refreshToken, String accessToken, String appId, String service, String tgt);

	/**
	 * 验证refreshToken有效性，无论有效性与否，都remove掉
	 * 
	 * @param refreshToken
	 * @return
	 */
	RefreshTokenContent validate(String refreshToken);
}
