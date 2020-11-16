package com.smart.sso.client.enums;

/**
 * Oauth2授权方式
 * 
 * @author Joe
 */
public enum GrantTypeEnum {
	
	AUTHORIZATION_CODE("authorization_code"), 
	PASSWORD("password");

	private String value;

	private GrantTypeEnum(String value) {
		this.value = value;
	}

	public String getValue() {
		return this.value;
	}
}