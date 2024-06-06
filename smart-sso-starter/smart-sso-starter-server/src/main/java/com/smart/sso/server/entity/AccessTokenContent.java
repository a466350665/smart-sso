package com.smart.sso.server.entity;

import com.smart.sso.base.entity.Userinfo;

public class AccessTokenContent {

	private CodeContent codeContent;
	private Userinfo userinfo;
	private String appId;

	public AccessTokenContent() {
	}

	public AccessTokenContent(CodeContent codeContent, Userinfo userinfo, String appId) {
		this.codeContent = codeContent;
		this.userinfo = userinfo;
		this.appId = appId;
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