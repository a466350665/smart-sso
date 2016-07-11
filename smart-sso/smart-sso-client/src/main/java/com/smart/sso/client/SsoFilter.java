package com.smart.sso.client;

import java.io.IOException;

import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.web.filter.authc.AuthenticatingFilter;
import org.apache.shiro.web.util.WebUtils;

import com.smart.sso.rpc.Permissionable;

/**
 * 自定义含验证码令牌
 * 
 * @author Joe
 */
public class SsoFilter extends AuthenticatingFilter {

	private String failureUrl;

	@Override
	protected AuthenticationToken createToken(ServletRequest request, ServletResponse response) throws Exception {
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		String ticket = httpRequest.getParameter(Permissionable.SSO_TOKEN_NAME);
		return new SsoToken(ticket);
	}

	@Override
	protected boolean onAccessDenied(ServletRequest request, ServletResponse response) throws Exception {
		
		Subject subject = getSubject(request, response);
		if (subject != null && subject.isAuthenticated()) {
			// 如果用户已经认证通过，直接跳转
			issueSuccessRedirect(request, response);
			return false;
		}
		else {
			return executeLogin(request, response);
		}
	}

	@Override
	protected boolean isAccessAllowed(ServletRequest request, ServletResponse response, Object mappedValue) {
		return false;
	}

	@Override
	protected boolean onLoginSuccess(AuthenticationToken token, Subject subject, ServletRequest request,
			ServletResponse response) throws Exception {
		issueSuccessRedirect(request, response);
		return false;
	}

	@Override
	protected boolean onLoginFailure(AuthenticationToken token, AuthenticationException ae, ServletRequest request,
			ServletResponse response) {
		// is user authenticated or in remember me mode ?
		Subject subject = getSubject(request, response);
		if (subject.isAuthenticated() || subject.isRemembered()) {
			try {
				issueSuccessRedirect(request, response);
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
		else {
			try {
				request.setAttribute("exception", ae.getMessage());
				WebUtils.issueRedirect(request, response, failureUrl);
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
		return false;
	}

	public String getFailureUrl() {
		return failureUrl;
	}

	public void setFailureUrl(String failureUrl) {
		this.failureUrl = failureUrl;
	}
}