package com.smart.sso.client.session;

import com.smart.sso.client.rpc.ClientAccessToken;
import com.smart.sso.client.rpc.ClientUser;

/**
 * 存Session中AccessToken
 * 
 * @author Joe
 */
public class SessionAccessToken extends ClientAccessToken {

	private static final long serialVersionUID = 4507869346123296527L;

	/**
	 * AccessToken过期时间
	 */
	private long expirationTime;

	public SessionAccessToken(String accessToken, int expiresIn, String refreshToken, ClientUser user,
			long expirationTime) {
		super(accessToken, expiresIn, refreshToken, user);
		this.expirationTime = expirationTime;
	}

	public long getExpirationTime() {
		return expirationTime;
	}

	public void setExpirationTime(long expirationTime) {
		this.expirationTime = expirationTime;
	}

	public boolean isExpired() {
		return System.currentTimeMillis() > expirationTime;
	}
}