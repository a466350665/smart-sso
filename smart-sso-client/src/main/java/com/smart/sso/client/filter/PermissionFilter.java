package com.smart.sso.client.filter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.util.SessionUtils;

/**
 * 权限控制Filter
 * 
 * @author Joe
 */
public class PermissionFilter extends ClientFilter {

	// 当前应用关联权限系统的应用编码
	private String ssoAppCode;
	// 当前应用配置的所有权限
	private Set<String> applicationPermissionSet;

	public PermissionFilter() {
	}

	public PermissionFilter(String ssoAppCode) {
		this.ssoAppCode = ssoAppCode;
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	    if (ssoAppCode == null || ssoAppCode.isEmpty()) {
			throw new IllegalArgumentException("ssoAppCode不能为空");
		}
		initApplicationPermissions();
	}

	/**
	 * 1.应用初始化，获取应用所有的菜单及权限
	 */
    public void initApplicationPermissions() {
        List<RpcPermissionDto> dbList = null;
        try {
            dbList = authenticationRpcService.selectApplicationPermissionList(ssoAppCode);
        } 
        catch (Exception e) {
            throw new IllegalArgumentException("无法连接到单点登录服务端,请检查配置sso.server.url", e);
        }

        applicationPermissionSet = dbList.stream().filter(p -> p.getUrl() != null && !p.getUrl().isEmpty())
            .map(p -> p.getUrl()).collect(Collectors.toSet());
    }

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String path = request.getServletPath();
		if (isPermitted(request, path)) {
			return true;
		}
		else {
			responseJson(response, SsoConstant.NO_PERMISSION, "没有访问权限");
			return false;
		}
	}

	private boolean isPermitted(HttpServletRequest request, String path) {
		Set<String> permissionSet = getLocalPermissionSet(request);
		if (permissionSet.contains(path)) {
			return true;
		}
		// 如果当前权限没有配置权限控制，也返回为True
		return !applicationPermissionSet.contains(path);
	}

	private Set<String> getLocalPermissionSet(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getPermission(request);
		if (sessionPermission == null) {
			sessionPermission = invokePermissionInSession(request);
		}
		return sessionPermission.getPermissionSet();
	}

	/**
	 * 保存权限信息
	 * 
	 * @param request
	 * @return
	 */
	public SessionPermission invokePermissionInSession(HttpServletRequest request) {
	    String token = SessionUtils.getToken(request);
		List<RpcPermissionDto> dbList = authenticationRpcService.selectUserPermissionList(token, ssoAppCode);

		List<RpcPermissionDto> menuList = new ArrayList<>();
		Set<String> operateSet = new HashSet<>();
		for (RpcPermissionDto menu : dbList) {
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
		Set<String> noPermissionSet = new HashSet<>(applicationPermissionSet);
		noPermissionSet.removeAll(operateSet);

		sessionPermission.setNoPermissions(String.join(",", noPermissionSet));

		// 保存登录用户权限列表
		sessionPermission.setPermissionSet(operateSet);
		SessionUtils.setPermission(request, sessionPermission);
		return sessionPermission;
	}

	public void setSsoAppCode(String ssoAppCode) {
		this.ssoAppCode = ssoAppCode;
	}
}