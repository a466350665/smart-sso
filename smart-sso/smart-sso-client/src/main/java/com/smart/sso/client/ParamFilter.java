package com.smart.sso.client;

import com.smart.sso.rpc.AuthenticationRpcService;

/**
 * 参数注入Filter
 * 
 * @author Joe
 */
public class ParamFilter {

	// 匹配路径（? 匹配1个字符，* 匹配0个或多个字符，** 中的0个或多个目录）
	protected String pattern;
	// 单点登录服务端URL
	protected String ssoServerUrl;
	// 单点登录服务端提供的RPC服务，由Spring容器注入
	protected AuthenticationRpcService authenticationRpcService;

	public void setSsoServerUrl(String ssoServerUrl) {
		this.ssoServerUrl = ssoServerUrl;
	}

	public void setPattern(String pattern) {
		this.pattern = pattern;
	}

	public String getPattern() {
		return pattern;
	}
	
	public void setAuthenticationRpcService(AuthenticationRpcService authenticationRpcService) {
		this.authenticationRpcService = authenticationRpcService;
	}
}