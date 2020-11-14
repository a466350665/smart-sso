package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.Expiration;
import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.enums.ClientTypeEnum;

/**
 * 刷新凭证refreshToken管理抽象
 * 
 * @author Joe
 */
public interface RefreshTokenManager extends Expiration {

	/**
	 * 生成refreshToken
	 * 
	 * @param tgt
	 * @param clientType
	 * @param redirectUri
	 * @param accessToken
	 * @param appId
	 * @return
	 */
	default String generate(String tgt, ClientTypeEnum clientType, String redirectUri, String accessToken,
			String appId) {
		String resfreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(resfreshToken, new RefreshTokenContent(tgt, clientType, redirectUri, accessToken, appId));
		return resfreshToken;
	}

	/**
	 * 生成refreshToken
	 * 
	 * @param refreshToken
	 * @param refreshTokenContent
	 */
	void create(String refreshToken, RefreshTokenContent refreshTokenContent);

	/**
	 * 验证refreshToken有效性，无论有效性与否，都remove掉
	 * 
	 * @param refreshToken
	 * @return
	 */
	RefreshTokenContent validate(String refreshToken);
}
