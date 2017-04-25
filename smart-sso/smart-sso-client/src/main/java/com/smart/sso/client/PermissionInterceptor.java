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
import com.smart.mvc.util.StringUtils;
import com.smart.sso.rpc.AuthenticationRpcService;
import com.smart.sso.rpc.RpcPermission;

public class PermissionInterceptor extends HandlerInterceptorAdapter {

	@Autowired
	private AuthenticationRpcService authenticationRpcService;

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws ServletException, IOException {
		String path = request.getServletPath();
		if (!PermissionInitServlet.getApplicationPermissionSet().contains(path)) {
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
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
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
		SessionUser user = SessionUtils.getSessionUser(request);
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
		Set<String> noPermissionSet = new HashSet<String>(PermissionInitServlet.getApplicationPermissionSet());
		noPermissionSet.removeAll(operateSet);
		sessionPermission.setNoPermissions(StringUtils.join(noPermissionSet.toArray(), ","));

		// 保存登录用户权限列表
		sessionPermission.setPermissionSet(operateSet);
		SessionUtils.setSessionPermission(request, sessionPermission);
		
		// 添加权限监控集合，当前session已更新最新权限
		if(PermissionMonitor.isChanged){
			PermissionMonitor.tokenSet.add(user.getToken());
		}
		return sessionPermission;
	}
}