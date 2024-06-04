package com.smart.sso.server.constant;

/**
 * @author Joe
 */
public class ServerConstant {

	// 存cookie中TGT名称，和Cas保存一致
	public static final String TGC = "TGC";

	// 登录页
	public static final String LOGIN_PATH = "/login";
	
	/**
	 * 服务端回调客户端地址参数名称
	 */
	public static final String REDIRECT_URI = "redirectUri";
    
    /**
     * 服务端单点登出回调客户端登出参数名称
     */
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";
}
