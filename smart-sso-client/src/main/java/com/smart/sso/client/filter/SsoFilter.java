package com.smart.sso.client.filter;

import java.io.IOException;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.RpcUserDto;
import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.SessionUtils;

/**
 * 单点登录及Token验证Filter
 * 
 * @author Joe
 */
public class SsoFilter extends ClientFilter {

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String token = SessionUtils.getToken(request);
		if (token == null) {
			token = request.getParameter(SsoConstant.SSO_TOKEN_NAME);
			if (token != null) {
				invokeAuthInfoInSession(request, token);
				// 为去掉URL中token参数，再跳转一次当前BackUrl
				response.sendRedirect(getRemoveTokenBackUrl(request));
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
	 * 存储sessionUser
	 * 
	 * @param request
	 * @return
	 * @throws IOException
	 */
	private void invokeAuthInfoInSession(HttpServletRequest request, String token) throws IOException {
        RpcUserDto rpcUser = authenticationRpcService.selectUser(token);
        if (rpcUser != null) {
            SessionUtils.setToken(request, token);
            SessionUtils.setUser(request, new SessionUser(rpcUser.getId(), rpcUser.getAccount()));
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
			responseJson(response, SsoConstant.NO_LOGIN, "未登录或已超时");
		}
		else {
			SessionUtils.invalidate(request);

			String ssoLoginUrl = new StringBuilder().append(ssoServerUrl)
					.append("/login?backUrl=").append(URLEncoder.encode(getBackUrl(request), "utf-8")).toString();

			response.sendRedirect(ssoLoginUrl);
		}
	}

	/**
	 * 去除返回地址中的token参数
	 * @param request
	 * @return
	 */
	private String getRemoveTokenBackUrl(HttpServletRequest request) {
		String backUrl = getBackUrl(request);
		return backUrl.substring(0, backUrl.indexOf(SsoConstant.SSO_TOKEN_NAME) - 1);
	}

	/**
	 * 返回地址
	 * @param request
	 * @return
	 */
	private String getBackUrl(HttpServletRequest request) {
		return new StringBuilder().append(request.getRequestURL())
				.append(request.getQueryString() == null ? "" : "?" + request.getQueryString()).toString();
	}
}