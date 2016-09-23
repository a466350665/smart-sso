package com.smart.sso.client;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.smart.mvc.config.ConfigUtils;
import com.smart.sso.rpc.AuthenticationRpcService;
import com.smart.sso.rpc.RpcUser;
import com.smart.util.StringUtils;

public class SsoInterceptor extends HandlerInterceptorAdapter {

	@Autowired
	private AuthenticationRpcService authenticationRpcService;

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws ServletException, IOException {
		String token = getToken(request);
		if (StringUtils.isNotBlank(token) && isLogined(token)) {
			return true;
		}
		request.getSession().invalidate();
		response.sendRedirect(ConfigUtils.getProperty("sso.login.url"));
		return false;
	}

	/**
	 * 获取Token
	 * 
	 * @param request
	 * @return
	 */
	private String getToken(HttpServletRequest request) {
		SessionUser sessionUser = ApplicationUtils.getSessionUser(request);
		String token;
		if (sessionUser != null) {
			token = sessionUser.getToken();
		}
		else {
			token = request.getParameter(ApplicationUtils.SSO_TOKEN_NAME);
			if (StringUtils.isNotBlank(token)) {
				RpcUser rpcUser = authenticationRpcService.findAuthInfo(token);
				if (rpcUser != null) {
					invokeAuthenticationInfoInSession(request, token, rpcUser.getUserName(),
							rpcUser.getProfile());
				}
			}
		}
		return token;
	}

	/**
	 * 保存认证信息到Session
	 * 
	 * @param token
	 * @param account
	 * @param profile
	 */
	private void invokeAuthenticationInfoInSession(HttpServletRequest request, String token, String account, Object profile) {
		ApplicationUtils.setSessionUser(request, new SessionUser(token, account, profile));
	}

	/**
	 * 是否已登录
	 * 
	 * @param token
	 * @return
	 */
	private boolean isLogined(String token) {
		return authenticationRpcService.validate(token);
	}
}