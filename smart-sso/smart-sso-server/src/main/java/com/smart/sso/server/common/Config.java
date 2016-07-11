package com.smart.sso.server.common;


/**
 * 应用配置信息
 * 
 * @author Joe
 */

public class Config {

	private String loginViewName = "/WEB-INF/view/login.jsp"; // 登录页面视图名称

	private int tokenTimeout = 30; // 令牌有效期，单位为分钟，默认30分钟

	public String getLoginViewName() {
		return loginViewName;
	}

	public void setLoginViewName(String loginViewName) {
		this.loginViewName = loginViewName;
	}

	/**
	 * 获取令牌有效期，单位为分钟
	 * 
	 * @return
	 */
	public int getTokenTimeout() {
		return tokenTimeout;
	}

	public void setTokenTimeout(int tokenTimeout) {
		this.tokenTimeout = tokenTimeout;
	}
}
