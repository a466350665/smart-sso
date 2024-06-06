package com.smart.sso.server.token;

import java.util.UUID;

import com.smart.sso.server.entity.AccessTokenContent;
import com.smart.sso.server.entity.Expiration;
import com.smart.sso.server.entity.RefreshTokenContent;

/**
 * 刷新凭证refreshToken管理抽象
 * 
 * @author Joe
 */
public interface RefreshTokenManager extends Expiration {

	/**
	 * 生成refreshToken
	 * 
	 * @param accessTokenContent
	 * @param accessToken
	 * @return
	 */
	default String generate(AccessTokenContent accessTokenContent, String accessToken) {
		String resfreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(resfreshToken, new RefreshTokenContent(accessTokenContent, accessToken));
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
