package com.smart.sso.client;

import javax.servlet.http.HttpServletRequest;

public class SessionUtils {
	
	/**
	 * sso授权回调参数token名称
	 */
	public static final String SSO_TOKEN_NAME = "__vt_param__";
	
	/**
	 * 登录用户
	 */
	public static final String SESSION_USER = "_sessionUser";

	/**
	 * 登录用户权限
	 */
	public static final String SESSION_USER_PERMISSION = "_sessionUserPermission";

	public static SessionUser getSessionUser(HttpServletRequest request) {
		return (SessionUser) request.getSession().getAttribute(SESSION_USER);
	}

	public static void setSessionUser(HttpServletRequest request, SessionUser sessionUser) {
		request.getSession().setAttribute(SESSION_USER, sessionUser);
	}

	public static SessionPermission getSessionPermission(HttpServletRequest request) {
		SessionUser user = SessionUtils.getSessionUser(request);
		if (PermissionMonitor.isChanged && !PermissionMonitor.tokenSet.contains(user.getToken()))
			return null;
		else
			return (SessionPermission) request.getSession().getAttribute(SESSION_USER_PERMISSION);
	}

	public static void setSessionPermission(HttpServletRequest request, SessionPermission sessionPermission) {
		request.getSession().setAttribute(SESSION_USER_PERMISSION, sessionPermission);
	}
}