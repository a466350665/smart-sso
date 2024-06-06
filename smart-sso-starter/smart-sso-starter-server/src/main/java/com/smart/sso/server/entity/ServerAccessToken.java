package com.smart.sso.server.entity;

/**
 * 服务端回传Token对象
 * 
 * @author Joe
 */
public class ServerAccessToken {

	/**
	 * 调用凭证
	 */
	private String accessToken;
	/**
	 * AccessToken超时时间，单位（秒）
	 */
	private int expiresIn;
	/**
	 * 当前AccessToken超时，用于刷新Token并延长服务端时效
	 */
	private String refreshToken;
	/**
	 * RefreshToken超时时间，单位（秒）
	 */
	private int refreshExpiresIn;
	/**
	 * 用户信息
	 */
	private ServerUser user;

	public ServerAccessToken() {
		super();
	}

	public ServerAccessToken(String accessToken, int expiresIn, String refreshToken, int refreshExpiresIn, ServerUser user) {
		super();
		this.accessToken = accessToken;
		this.expiresIn = expiresIn;
		this.refreshToken = refreshToken;
		this.refreshExpiresIn = refreshExpiresIn;
		this.user = user;
	}

	public String getAccessToken() {
		return accessToken;
	}

	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	public int getExpiresIn() {
		return expiresIn;
	}

	public void setExpiresIn(int expiresIn) {
		this.expiresIn = expiresIn;
	}

	public String getRefreshToken() {
		return refreshToken;
	}

	public void setRefreshToken(String refreshToken) {
		this.refreshToken = refreshToken;
	}

	public int getRefreshExpiresIn() {
		return refreshExpiresIn;
	}

	public void setRefreshExpiresIn(int refreshExpiresIn) {
		this.refreshExpiresIn = refreshExpiresIn;
	}
	
	public ServerUser getUser() {
		return user;
	}

	public void setUser(ServerUser user) {
		this.user = user;
	}
}