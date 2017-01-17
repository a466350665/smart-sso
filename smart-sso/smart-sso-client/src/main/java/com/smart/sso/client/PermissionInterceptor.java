package com.smart.sso.client;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.exception.ServiceException;
import com.smart.mvc.model.ResultCode;
import com.smart.sso.rpc.AuthenticationRpcService;
import com.smart.sso.rpc.RpcPermission;
import com.smart.util.StringUtils;

public class PermissionInterceptor extends HandlerInterceptorAdapter {

	@Autowired
	private AuthenticationRpcService authenticationRpcService;

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws ServletException, IOException {
		String path = request.getServletPath();
		Set<String> applicationPermissionSet = ApplicationUtils.getApplicationPermission(request);
		if (!applicationPermissionSet.contains(path)) {
			return true;
		}
		if (isPermitted(request, path)) {
			return true;
		}
		throw new ServiceException(ResultCode.SSO_PERMISSION_ERROR, "未登录或已超时");
	}

	private boolean isPermitted(HttpServletRequest request, String path) {
		Set<String> permissionSet = getLocalPermissionSet(request);
		return permissionSet.contains(path);
	}

	private Set<String> getLocalPermissionSet(HttpServletRequest request) {
		SessionPermission sessionPermission = ApplicationUtils.getSessionPermission(request);
		if (sessionPermission == null) {
			sessionPermission = invokePermissionInSession(request);
		}
		return sessionPermission.getPermissionSet();
	}

	/**
	 * 保存权限信息
	 * 
	 * @param token
	 * @return
	 */
	public SessionPermission invokePermissionInSession(HttpServletRequest request) {
		SessionUser user = (SessionUser) request.getSession().getAttribute("_sessionUser");
		List<RpcPermission> dbList = authenticationRpcService.findPermissionList(user.getToken(),
				ConfigUtils.getProperty("sso.app.code"));

		List<RpcPermission> menuList = new ArrayList<RpcPermission>();
		Set<String> operateSet = new HashSet<String>();
		for (RpcPermission menu : dbList) {
			if (menu.getIsMenu()) {
				menuList.add(menu);
			}
			if (menu.getUrl() != null) {
				operateSet.add(menu.getUrl());
			}
		}

		SessionPermission sessionPermission = new SessionPermission();
		// 设置登录用户菜单列表
		sessionPermission.setMenuList(menuList);

		// 保存登录用户没有权限的URL，方便前端去隐藏相应操作按钮
		Set<String> noPermissionSet = new HashSet<String>(ApplicationUtils.getApplicationPermission(request));
		noPermissionSet.removeAll(operateSet);
		sessionPermission.setNoPermissions(StringUtils.join(noPermissionSet.toArray(), ","));

		// 保存登录用户权限列表
		sessionPermission.setPermissionSet(operateSet);
		ApplicationUtils.setSessionPermission(request, sessionPermission);
		return sessionPermission;
	}
}