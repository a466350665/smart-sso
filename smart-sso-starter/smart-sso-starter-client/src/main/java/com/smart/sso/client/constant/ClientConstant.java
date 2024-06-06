package com.smart.sso.client.constant;

/**
 * @author Joe
 */
public class ClientConstant {
	
	/**
     * 服务端登录地址
     */
	public static final String LOGIN_URL = "/login";

	/**
	 * 服务端登出地址
	 */
	public static final String LOGOUT_URL = "/logout";
	
	/**
	 * 服务端回调客户端地址参数名称
	 */
	public static final String REDIRECT_URI = "redirectUri";
    
    /**
     * 服务端单点登出回调客户端登出参数名称
     */
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";
    
    /**
     * 模糊匹配后缀
     */
    public static final String URL_FUZZY_MATCH = "/*";

	/**
	 * 存cookie中ST名称，和CAS概念保存一致
	 */
	public static final String COOKIE_SERVICE_TICKET = "ST";
	
	/**
	 * 未登录或已过期
	 */
	public static final int NO_LOGIN = 2100;
}
