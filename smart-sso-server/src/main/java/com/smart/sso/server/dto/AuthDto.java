package com.smart.sso.server.dto;

import java.io.Serializable;

import com.smart.sso.client.rpc.SsoUser;
import com.smart.sso.server.common.AuthContent;

public class AuthDto implements Serializable {

	private static final long serialVersionUID = 4587667812642196058L;

	private AuthContent authContent;
	private SsoUser user;

	public AuthDto(AuthContent authContent, SsoUser user) {
		this.authContent = authContent;
		this.user = user;
	}

	public AuthContent getAuthContent() {
		return authContent;
	}

	public void setAuthContent(AuthContent authContent) {
		this.authContent = authContent;
	}

	public SsoUser getUser() {
		return user;
	}

	public void setUser(SsoUser user) {
		this.user = user;
	}
}