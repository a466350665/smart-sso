package com.smart.sso.client;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.smart.sso.rpc.RpcUser;

/**
 * 单点登录及Token验证Filter
 * 
 * @author Joe
 */
public class SsoFilter extends ClientFilter {

	// sso授权回调参数token名称
	public static final String SSO_TOKEN_NAME = "__vt_param__";

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String token = getLocalToken(request);
		if (token == null) {
			token = request.getParameter(SSO_TOKEN_NAME);
			if (token != null) {
				invokeAuthInfoInSession(request, token);
				// 再跳转一次当前URL，以便去掉URL中token参数
				response.sendRedirect(request.getRequestURL().toString());
				return false;
			}
		}
		else if (authenticationRpcService.validate(token)) {// 验证token是否有效
			return true;
		}
		redirectLogin(request, response);
		return false;
	}
	
	/**
	 * 获取Session中token
	 * 
	 * @param request
	 * @return
	 */
	private String getLocalToken(HttpServletRequest request) {
		SessionUser sessionUser = SessionUtils.getSessionUser(request);
		return sessionUser == null ? null : sessionUser.getToken();
	}


	/**
	 * 存储sessionUser
	 * 
	 * @param request
	 * @return
	 * @throws IOException
	 */
	private void invokeAuthInfoInSession(HttpServletRequest request, String token) throws IOException {
		RpcUser rpcUser = authenticationRpcService.findAuthInfo(token);
		if (rpcUser != null) {
			SessionUtils.setSessionUser(request, new SessionUser(token, rpcUser.getAccount()));
		}
	}

	/**
	 * 跳转登录
	 * 
	 * @param request
	 * @param response
	 * @throws IOException
	 */
	private void redirectLogin(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (isAjaxRequest(request)) {
			responseJson(response, SsoResultCode.SSO_TOKEN_ERROR, "未登录或已超时");
		}
		else {
			SessionUtils.invalidate(request);
			String ssoLoginUrl = new StringBuilder().append(isServer ? request.getContextPath() : ssoServerUrl)
					.append("/login?backUrl=").append(request.getRequestURL()).toString();

			response.sendRedirect(ssoLoginUrl);
		}
	}
}