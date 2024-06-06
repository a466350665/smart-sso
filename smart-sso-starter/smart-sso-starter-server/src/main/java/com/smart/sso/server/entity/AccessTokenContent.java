package com.smart.sso.server.entity;

public class AccessTokenContent {

	private CodeContent codeContent;
	private ServerUser user;
	private String appId;

	public AccessTokenContent() {
	}

	public AccessTokenContent(CodeContent codeContent, ServerUser user, String appId) {
		this.codeContent = codeContent;
		this.user = user;
		this.appId = appId;
	}

	public CodeContent getCodeContent() {
		return codeContent;
	}

	public void setCodeContent(CodeContent codeContent) {
		this.codeContent = codeContent;
	}

	public ServerUser getUser() {
		return user;
	}

	public void setUser(ServerUser user) {
		this.user = user;
	}

	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}
}