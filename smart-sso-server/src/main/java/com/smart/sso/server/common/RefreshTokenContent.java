package com.smart.sso.server.common;

public class RefreshTokenContent extends AccessTokenContent {

	private static final long serialVersionUID = -1332598459045608781L;

	private String accessToken;
	
	private String appId;

	public RefreshTokenContent(String service, String tgt, String accessToken, String appId) {
		super(service, tgt);
		this.accessToken = accessToken;
		this.appId = appId;
	}

	public String getAccessToken() {
		return accessToken;
	}

	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}
}