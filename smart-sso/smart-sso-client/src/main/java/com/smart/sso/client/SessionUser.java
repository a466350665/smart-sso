package com.smart.sso.client;

import java.io.Serializable;

public class SessionUser implements Serializable{

	private static final long serialVersionUID = 1764365572138947234L;
	
	// 登录用户访问Token
	private String token;
	// 登录名
	private String account;
	// 登录对象
	private Object profile;

	public SessionUser() {
		super();
	}

	public SessionUser(String token, String account, Object profile) {
		super();
		this.token = token;
		this.account = account;
		this.profile = profile;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public String getAccount() {
		return account;
	}

	public void setAccount(String account) {
		this.account = account;
	}

	public Object getProfile() {
		return profile;
	}

	public void setProfile(Object profile) {
		this.profile = profile;
	}
}
