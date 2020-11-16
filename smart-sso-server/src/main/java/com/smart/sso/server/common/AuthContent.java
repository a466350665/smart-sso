package com.smart.sso.server.common;

import java.io.Serializable;

import com.smart.sso.server.enums.ClientTypeEnum;

/**
 * 授权存储信息
 * 
 * @author Joe
 */
public class AuthContent implements Serializable {

	private static final long serialVersionUID = -1332598459045608781L;

	private String tgt;
	private ClientTypeEnum clientType;
	private String redirectUri;

	public AuthContent(String tgt, ClientTypeEnum clientType, String redirectUri) {
		this.tgt = tgt;
		this.clientType = clientType;
		this.redirectUri = redirectUri;
	}

	public String getTgt() {
		return tgt;
	}

	public void setTgt(String tgt) {
		this.tgt = tgt;
	}

	public ClientTypeEnum getClientType() {
		return clientType;
	}

	public void setClientType(ClientTypeEnum clientType) {
		this.clientType = clientType;
	}

	public String getRedirectUri() {
		return redirectUri;
	}

	public void setRedirectUri(String redirectUri) {
		this.redirectUri = redirectUri;
	}
}