package com.smart.sso.client.filter;

/**
 * 参数注入Filter
 * 
 * @author Joe
 */
public class ParamFilter {

	// 单点登录服务端URL
	protected String ssoServerUrl;

	public void setSsoServerUrl(String ssoServerUrl) {
		this.ssoServerUrl = ssoServerUrl;
	}
	
	public String getSsoServerUrl() {
		return ssoServerUrl;
	}
}