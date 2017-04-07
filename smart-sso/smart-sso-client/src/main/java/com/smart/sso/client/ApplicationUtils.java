package com.smart.sso.client;

import java.util.List;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import com.smart.mvc.util.StringUtils;
import com.smart.sso.rpc.RpcPermission;

@SuppressWarnings("unchecked")
public class ApplicationUtils {
	
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
	 * 登录用户
	 */
	public static final String SESSION_USER = "_sessionUser";

	/**
	 * 登录用户权限
	 */
	public static final String SESSION_USER_PERMISSION = "_sessionUserPermission";

	public static Set<String> getApplicationPermission(HttpServletRequest request) {
		return (Set<String>) request.getServletContext().getAttribute(APPLICATION_PERMISSION);
	}

	public static void setApplicationPermission(ServletContext servletContext, Set<String> applicationPermissionSet) {
		servletContext.setAttribute(APPLICATION_PERMISSION, applicationPermissionSet);
	}

	public static List<RpcPermission> getApplicationMenu(HttpServletRequest request) {
		return (List<RpcPermission>) request.getServletContext().getAttribute(APPLICATION_MENU);
	}

	public static void setApplicationMenu(ServletContext servletContext, List<RpcPermission> applicationMenuList) {
		servletContext.setAttribute(APPLICATION_MENU, applicationMenuList);
	}

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

	public static String getIpAddr(HttpServletRequest request) {
		String ip = request.getHeader("X-Real-IP");
		if (!StringUtils.isBlank(ip) && !"unknown".equalsIgnoreCase(ip)) {
			return ip;
		}
		ip = request.getHeader("X-Forwarded-For");
		if (!StringUtils.isBlank(ip) && !"unknown".equalsIgnoreCase(ip)) {
			// 多次反向代理后会有多个IP值，第一个为真实IP。
			int index = ip.indexOf(',');
			if (index != -1) {
				return ip.substring(0, index);
			}
			else {
				return ip;
			}
		}
		else {
			return request.getRemoteAddr();
		}
	}
}