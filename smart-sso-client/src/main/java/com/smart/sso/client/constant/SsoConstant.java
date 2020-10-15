package com.smart.sso.client.constant;

/**
 * 单点登录权限返回码
 * 
 * @author Joe
 */
public class SsoConstant {
    
    /**
     * 服务端认证回调票据参数ticket名称
     */
    public static final String TICKET_PARAMETER_NAME = "ticket";
    
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";
    
    /** 模糊匹配后缀 */
    public static final String URL_FUZZY_MATCH = "/*";
    
    /**
     * 用户信息
     */
    public static final String SESSION_USER = "_sessionUser";
	
	public static final int NO_LOGIN = 2100;// 未登录或已过期
}
