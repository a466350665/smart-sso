package com.smart.sso.base.entity;

import java.io.Serializable;

/**
 * 服务端回传Token对象
 * 
 * @author Joe
 */
public class AccessToken implements Serializable {

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
	 * 当前AccessToken超时，用于刷新AccessToken并延长服务端时效
	 */
	private String refreshToken;
	/**
	 * RefreshToken超时时间，单位（秒）
	 */
	private int refreshExpiresIn;
	/**
	 * 用户信息
	 */
	private Userinfo userinfo;

	public AccessToken() {
		super();
	}

	public AccessToken(String accessToken, int expiresIn, String refreshToken, int refreshExpiresIn, Userinfo userinfo) {
		super();
		this.accessToken = accessToken;
		this.expiresIn = expiresIn;
		this.refreshToken = refreshToken;
		this.refreshExpiresIn = refreshExpiresIn;
		this.userinfo = userinfo;
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

	public Userinfo getUserinfo() {
		return userinfo;
	}

	public void setUserinfo(Userinfo userinfo) {
		this.userinfo = userinfo;
	}
}