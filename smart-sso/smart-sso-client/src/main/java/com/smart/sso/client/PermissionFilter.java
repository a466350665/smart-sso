package com.smart.sso.client;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.util.StringUtils;

import com.smart.sso.rpc.RpcPermission;

/**
 * 权限控制Filter
 * 
 * @author Joe
 */
public class PermissionFilter extends ClientFilter {

	// 当前应用关联权限系统的应用编码
	private String ssoAppCode;

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		if (StringUtils.isEmpty(ssoAppCode)) {
			throw new IllegalArgumentException("ssoAppCode不能为空");
		}
		ApplicationPermission.initApplicationPermissions(authenticationRpcService, ssoAppCode);
	}

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String path = request.getServletPath();
		if (isPermitted(request, path))
			return true;
		else if (!ApplicationPermission.getApplicationPermissionSet().contains(path))
			return true;
		else {
			responseJson(response, SsoResultCode.SSO_PERMISSION_ERROR, "没有访问权限");
			return false;
		}
	}

	private boolean isPermitted(HttpServletRequest request, String path) {
		Set<String> permissionSet = getLocalPermissionSet(request);
		return permissionSet.contains(path);
	}

	private Set<String> getLocalPermissionSet(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		if (sessionPermission == null || sessionPermissionChanged(request)) {
			sessionPermission = invokePermissionInSession(request);
		}
		return sessionPermission.getPermissionSet();
	}

	private boolean sessionPermissionChanged(HttpServletRequest request) {
		SessionUser user = SessionUtils.getSessionUser(request);
		return PermissionJmsMonitor.isChanged && !PermissionJmsMonitor.tokenSet.contains(user.getToken());
	}

	/**
	 * 保存权限信息
	 * 
	 * @param token
	 * @return
	 */
	public SessionPermission invokePermissionInSession(HttpServletRequest request) {
		SessionUser user = SessionUtils.getSessionUser(request);
		List<RpcPermission> dbList = authenticationRpcService.findPermissionList(user.getToken(), ssoAppCode);

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
		Set<String> noPermissionSet = new HashSet<String>(ApplicationPermission.getApplicationPermissionSet());
		noPermissionSet.removeAll(operateSet);

		sessionPermission.setNoPermissions(StringUtils.arrayToDelimitedString(noPermissionSet.toArray(), ","));

		// 保存登录用户权限列表
		sessionPermission.setPermissionSet(operateSet);
		SessionUtils.setSessionPermission(request, sessionPermission);

		// 添加权限监控集合，当前session已更新最新权限
		if (PermissionJmsMonitor.isChanged) {
			PermissionJmsMonitor.tokenSet.add(user.getToken());
		}
		return sessionPermission;
	}

	public void setSsoAppCode(String ssoAppCode) {
		this.ssoAppCode = ssoAppCode;
	}
}