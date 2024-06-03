package com.smart.sso.server.common;

import java.io.Serializable;

import com.smart.sso.client.rpc.SsoUser;

public class AccessTokenContent implements Serializable {

	private static final long serialVersionUID = 4587667812642196058L;

	private CodeContent codeContent;
	private SsoUser user;
	private String appId;

	public AccessTokenContent(CodeContent codeContent, SsoUser user, String appId) {
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

	public SsoUser getUser() {
		return user;
	}

	public void setUser(SsoUser user) {
		this.user = user;
	}

	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}
}