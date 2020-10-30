package com.smart.sso.client.session;

import com.smart.sso.client.rpc.RpcAccessToken;

/**
 * 存Session中AccessToken
 * 
 * @author Joe
 */
public class SessionAccessToken extends RpcAccessToken {

	private static final long serialVersionUID = 4507869346123296527L;

	/**
	 * AccessToken过期时间
	 */
	private long expirationTime;

	/**
	 * 用户信息
	 */
	private SessionUser user;

	public SessionAccessToken(String accessToken, int expiresIn, String refreshToken, long expirationTime,
			SessionUser user) {
		super(accessToken, expiresIn, refreshToken);
		this.expirationTime = expirationTime;
		this.user = user;
	}

	public long getExpirationTime() {
		return expirationTime;
	}

	public void setExpirationTime(long expirationTime) {
		this.expirationTime = expirationTime;
	}

	public SessionUser getUser() {
		return user;
	}

	public void setUser(SessionUser user) {
		this.user = user;
	}

	public boolean isExpired() {
		return System.currentTimeMillis() > expirationTime;
	}
}