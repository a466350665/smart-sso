package com.smart.sso.server.entity;

import com.smart.sso.base.entity.Userinfo;

public class TokenContent {

	private String accessToken;
	private String refreshToken;
	private CodeContent codeContent;
	private Userinfo userinfo;
	private String appId;

	public TokenContent() {
	}

	public TokenContent(String accessToken, String refreshToken, CodeContent codeContent, Userinfo userinfo, String appId) {
		this.accessToken = accessToken;
		this.refreshToken = refreshToken;
		this.codeContent = codeContent;
		this.userinfo = userinfo;
		this.appId = appId;
	}

	public String getAccessToken() {
		return accessToken;
	}

	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	public String getRefreshToken() {
		return refreshToken;
	}

	public void setRefreshToken(String refreshToken) {
		this.refreshToken = refreshToken;
	}

	public CodeContent getCodeContent() {
		return codeContent;
	}

	public void setCodeContent(CodeContent codeContent) {
		this.codeContent = codeContent;
	}

	public Userinfo getUserinfo() {
		return userinfo;
	}

	public void setUserinfo(Userinfo userinfo) {
		this.userinfo = userinfo;
	}

	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}
}