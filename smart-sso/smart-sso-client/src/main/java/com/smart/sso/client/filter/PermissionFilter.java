package com.smart.sso.client.filter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.model.RpcPermission;
import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.util.SessionUtils;

/**
 * 权限控制Filter
 * 
 * @author Joe
 */
public class PermissionFilter extends ClientFilter {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	// 当前应用关联权限系统的应用编码
	private String ssoAppCode;
	// 存储已获取最新权限的token集合，当权限发生变动时清空
	private Set<String> sessionPermissionCache = new CopyOnWriteArraySet<>();
	// 应用所有权限URL
	private Set<String> applicationPermissionSet;
	// 应用所有权限并发锁
	private final Object applicationPermissionMonitor = new Object();

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
	 * 1.应用初始化，获取应用所有的菜单及权限 2.权限有变动修改，JMS通知重新加载
	 */
	public void initApplicationPermissions() {
		List<RpcPermission> dbList = null;
		try {
			dbList = authenticationRpcService.selectPermissionList(null, ssoAppCode);
		}
		catch (Exception e) {
			dbList = Collections.emptyList();
			logger.error("无法连接到单点登录服务端,请检查配置sso.server.url", e);
		}

		synchronized (applicationPermissionMonitor) {
			applicationPermissionSet = new HashSet<>();
            for (RpcPermission menu : dbList) {
                if (!(menu.getUrl() == null || menu.getUrl().isEmpty())) {
                    applicationPermissionSet.add(menu.getUrl());
                }
            }
		}
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
		synchronized (applicationPermissionMonitor) {
			return !applicationPermissionSet.contains(path);
		}
	}

	private Set<String> getLocalPermissionSet(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		String token = SessionUtils.getSessionUser(request).getToken();
		if (sessionPermission == null || !sessionPermissionCache.contains(token)) {
			sessionPermission = invokePermissionInSession(request, token);
		}
		return sessionPermission.getPermissionSet();
	}

	/**
	 * 保存权限信息
	 * 
	 * @param token
	 * @return
	 */
	public SessionPermission invokePermissionInSession(HttpServletRequest request, String token) {
		List<RpcPermission> dbList = authenticationRpcService.selectPermissionList(token, ssoAppCode);

		List<RpcPermission> menuList = new ArrayList<>();
		Set<String> operateSet = new HashSet<>();
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
		Set<String> noPermissionSet;
		synchronized (applicationPermissionMonitor) {
			noPermissionSet = new HashSet<>(applicationPermissionSet);
		}
		noPermissionSet.removeAll(operateSet);

		sessionPermission.setNoPermissions(String.join(",", noPermissionSet));

		// 保存登录用户权限列表
		sessionPermission.setPermissionSet(operateSet);
		SessionUtils.setSessionPermission(request, sessionPermission);

		// 添加权限监控集合，当前session已更新最新权限
		sessionPermissionCache.add(token);
		return sessionPermission;
	}

	public void setSsoAppCode(String ssoAppCode) {
		this.ssoAppCode = ssoAppCode;
	}

	/**
	 * 失效session权限缓存
	 */
	public void invalidateSessionPermissions() {
		sessionPermissionCache.clear();
	}
}