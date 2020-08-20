package com.smart.sso.client.util;

import javax.servlet.http.HttpServletRequest;

import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.model.SessionUser;

/**
 * 当前已登录用户Session工具
 * 
 * @author Joe
 */
public class SessionUtils {
	/**
	 * 用户信息
	 */
	public static final String SESSION_USER = "_sessionUser";

	/**
	 * 用户权限
	 */
	public static final String SESSION_USER_PERMISSION = "_sessionUserPermission";

	public static SessionUser getSessionUser(HttpServletRequest request) {
		return (SessionUser) request.getSession().getAttribute(SESSION_USER);
	}

	public static void setSessionUser(HttpServletRequest request, SessionUser sessionUser) {
		request.getSession().setAttribute(SESSION_USER, sessionUser);
	}

	public static SessionPermission getSessionPermission(HttpServletRequest request) {
		return (SessionPermission) request.getSession().getAttribute(SESSION_USER_PERMISSION);
	}

	public static void setSessionPermission(HttpServletRequest request, SessionPermission sessionPermission) {
	    request.getSession().setAttribute(SESSION_USER_PERMISSION, sessionPermission);
	}
	
	public static void invalidate(HttpServletRequest request){
		setSessionUser(request, null);
		setSessionPermission(request, null);
	}
}