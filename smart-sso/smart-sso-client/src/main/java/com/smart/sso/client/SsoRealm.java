package com.smart.sso.client;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authc.SimpleAuthenticationInfo;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.authz.SimpleAuthorizationInfo;
import org.apache.shiro.cache.Cache;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.web.subject.WebSubject;
import org.springframework.beans.factory.annotation.Autowired;

import com.smart.mvc.config.ConfigUtils;
import com.smart.sso.rpc.AuthenticationRpcService;
import com.smart.sso.rpc.Menu;
import com.smart.sso.rpc.Permissionable;
import com.smart.sso.rpc.RpcUser;
import com.smart.util.StringUtils;

/**
 * 自定义Shiro验证授权
 * 
 * @author Joe
 */
public class SsoRealm extends AuthorizingRealm {
	
	/**
	 * 应用权限是否改动，PermissionJmsListener监听修改
	 */
	private volatile boolean allPermissionSetChanged = false;
	private final Object allPermissionSetChangedMonitor = new Object();
	
	@Autowired
	private AuthenticationRpcService authenticationRpcService;

	public SsoRealm() {
		setAuthenticationTokenClass(SsoToken.class);
	}

	/**
	 * 登陆认证
	 */
	@Override
	protected AuthenticationInfo doGetAuthenticationInfo(AuthenticationToken token) throws AuthenticationException {
		SsoToken ssoToken = (SsoToken) token;
		RpcUser rpcUser = authenticationRpcService.findAuthInfo(ssoToken.getCredentials().toString(),
				ConfigUtils.getProperty("app.code"));
		if (rpcUser != null) {
			invokeAuthenticationInfoInSession(ssoToken.getCredentials().toString(), rpcUser.getUserName(),
					rpcUser.getProfile());
			return new SimpleAuthenticationInfo(ssoToken.getPrincipal(), ssoToken.getCredentials(), getName());
		}
		else {
			throw new UnknownAccountException();
		}
	}

	/**
	 * 保存认证信息到Session
	 * 
	 * @param token
	 * @param account
	 * @param profile
	 */
	private void invokeAuthenticationInfoInSession(String token, String account, Object profile) {
		ServletRequest request = ((WebSubject) SecurityUtils.getSubject()).getServletRequest();
		HttpSession session = ((HttpServletRequest) request).getSession();
		session.setAttribute(Permissionable.SESSION_TOKEN, token);
		session.setAttribute(Permissionable.SESSION_ACCOUNT, account);
		session.setAttribute(Permissionable.SESSION_PROFILE, profile);
	}

	/**
	 * 授权方法，在配有缓存的情况下，只加载一次
	 */
	protected AuthorizationInfo doGetAuthorizationInfo(PrincipalCollection principals) {
		SimpleAuthorizationInfo info = new SimpleAuthorizationInfo();
		info.setStringPermissions(invokePermissionInSession(principals.getPrimaryPrincipal().toString()));
		return info;
	}

	/**
	 * 保存权限信息
	 * 
	 * @param token
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Set<String> invokePermissionInSession(String token) {
		List<Menu> dbList = authenticationRpcService.findPermissionList(token, ConfigUtils.getProperty("app.code"));

		List<Menu> menuList = new ArrayList<Menu>();
		Set<String> operateSet = new HashSet<String>();
		for (Menu menu : dbList) {
			if (menu.getIsMenu()) {
				menuList.add(menu);
			}
			if (menu.getUrl() != null) {
				operateSet.add(menu.getUrl());
			}
		}

		// 保存登录用户列表
		ServletRequest request = ((WebSubject) SecurityUtils.getSubject()).getServletRequest();
		HttpSession session = ((HttpServletRequest) request).getSession();
		session.setAttribute(Permissionable.SESSION_USER_MENU, menuList);

		if (allPermissionSetChanged) {// 应用权限如果被修改，重新加载一遍
			synchronized (this.allPermissionSetChangedMonitor) {
				PermissionListener.initApplicationPermissions(request.getServletContext());
				allPermissionSetChanged = false;
			}
		}
		Set<String> allPermissionSet = (Set<String>) request.getServletContext().getAttribute(Permissionable.APPLICATION_PERMISSION);
		
		// 保存登录用户没有权限的URL，方便前端去隐藏相应操作按钮
		Set<String> noPermissionSet = new HashSet<String>(allPermissionSet);
		noPermissionSet.removeAll(operateSet);
		session.setAttribute(Permissionable.SESSION_USER_NO_PERMISSION,
				StringUtils.join(noPermissionSet.toArray(), ","));
		return operateSet;
	}

	/**
	 * 清空应用权限
	 */
	public void clearApplicationPermissions() {
		this.allPermissionSetChanged = true;
	}

	/**
	 * 权限变动，清除所有用户授权信息缓存，重新加载
	 */
	public void clearAllCachedAuthorizationInfo() {
		Cache<Object, AuthorizationInfo> cache = getAuthorizationCache();
		if (cache != null) {
			for (Object key : cache.keys()) {
				cache.remove(key);
			}
		}
	}
}