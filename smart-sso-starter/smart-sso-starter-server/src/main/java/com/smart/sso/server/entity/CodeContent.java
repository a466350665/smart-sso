package com.smart.sso.server.entity;

/**
 * 授权存储信息
 * 
 * @author Joe
 */
public class CodeContent {

	private String tgt;
	private String redirectUri;

	public CodeContent() {
	}

	public CodeContent(String tgt, String redirectUri) {
		this.tgt = tgt;
		this.redirectUri = redirectUri;
	}

	public String getTgt() {
		return tgt;
	}

	public void setTgt(String tgt) {
		this.tgt = tgt;
	}

	public String getRedirectUri() {
		return redirectUri;
	}

	public void setRedirectUri(String redirectUri) {
		this.redirectUri = redirectUri;
	}
}