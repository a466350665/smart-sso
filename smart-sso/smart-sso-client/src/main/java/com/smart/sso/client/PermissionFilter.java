package com.smart.sso.client;

import java.io.IOException;
import java.util.Set;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.shiro.subject.Subject;
import org.apache.shiro.web.filter.authz.AuthorizationFilter;

import com.smart.sso.rpc.Permissionable;

/**
 * 基于URL的权限判断过滤器
 * 
 * @author Joe
 */
public class PermissionFilter extends AuthorizationFilter {
	/**
	 * 控制只有配置权限拦截的URL才进行拦截
	 */
	@SuppressWarnings("unchecked")
	public boolean isAccessAllowed(ServletRequest request, ServletResponse response, Object mappedValue)
			throws IOException {
		Subject subject = getSubject(request, response);
		String path = ((HttpServletRequest) request).getServletPath();
		Set<String> allPermissionSet = (Set<String>) request.getServletContext().getAttribute(
				Permissionable.APPLICATION_PERMISSION);
		boolean isPermitted = true;
		if (allPermissionSet.contains(path) && !subject.isPermitted(path)) {
			isPermitted = false;
		}
		return isPermitted;
	}
}
