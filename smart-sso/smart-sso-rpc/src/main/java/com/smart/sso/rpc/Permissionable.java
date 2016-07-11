package com.smart.sso.rpc;

/**
 * 权限结构
 * 
 * @author Joe
 */
public interface Permissionable {
	
	/**
	 * sso授权回调参数token名称
	 */
	public static final String SSO_TOKEN_NAME = "__vt_param__";
	
	/**
	 * 系统所有菜单
	 */
	public static final String APPLICATION_MENU = "_applicationMenu";
	
	/**
	 * 系统所有权限
	 */
	public static final String APPLICATION_PERMISSION = "_applicationPermission";

	/**
	 * 登录用户没有的权限
	 */
	public static final String SESSION_USER_NO_PERMISSION = "_sessionUserNoPermission";
	
	/**
	 * 登录用户菜单
	 */
	public static final String SESSION_USER_MENU = "_sessionUserMenu";
	
	/**
	 * 登录用户权限
	 */
	public static final String SESSION_USER_PERMISSION = "_sessionUserPermission";
	
	/**
	 * 登录用户访问Token
	 */
	public static final String SESSION_TOKEN = "_sessionToken";
	
	/**
	 * 登录名
	 */
	public static final String SESSION_ACCOUNT = "_sessionAccount";
	
	/**
	 * 登录对象
	 */
	public static final String SESSION_PROFILE = "_sessionProfile";
}
