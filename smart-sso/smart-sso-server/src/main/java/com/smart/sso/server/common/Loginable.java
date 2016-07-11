package com.smart.sso.server.common;

/**
 * 登录结构
 * 
 * @author Joe
 */
public interface Loginable {
	
	//登录出错消息
	public static final String VALIDATE_MESSAGE_NAME = "errorMessage";
	
	//登录页
	public static final String LOGIN_PATH = "/login";
	
	//登录成功页
	public static final String LOGIN_SUCCESS_PATH = "redirect:/index";
}
