package com.smart.sso.client;

import org.apache.shiro.authc.AuthenticationToken;

/**
 * 自定义含验证码令牌
 * 
 * @author Joe
 */
public class SsoToken implements AuthenticationToken {

	private static final long serialVersionUID = -2441985030934916003L;

	private String token = null;
	
	public SsoToken(String token) {
		this.token = token;
	}

	@Override
	public Object getCredentials() {
		return token;
	}

	@Override
	public Object getPrincipal() {
        return token;
    }
}