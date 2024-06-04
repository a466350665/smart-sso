package com.smart.sso.server.common;

import java.io.Serializable;

/**
 * 服务端回传Token对象
 * 
 * @author Joe
 */
public class ServerAccessToken implements Serializable {

	private static final long serialVersionUID = 4507869346123296527L;

	/**
	 * 调用凭证
	 */
	private String accessToken;
	/**
	 * AccessToken超时时间，单位（秒）
	 */
	private int expiresIn;
	/**
	 * 当前AccessToken超时，用于刷新AccessToken并延长服务端session时效必要参数
	 */
	private String refreshToken;
	/**
	 * 用户信息
	 */
	private ServerUser user;

	public ServerAccessToken(String accessToken, int expiresIn, String refreshToken, ServerUser user) {
		super();
		this.accessToken = accessToken;
		this.expiresIn = expiresIn;
		this.refreshToken = refreshToken;
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
	
	public ServerUser getUser() {
		return user;
	}

	public void setUser(ServerUser user) {
		this.user = user;
	}
}