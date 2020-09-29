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
 * 单点登录ticket验证Filter
 * 
 * @author Joe
 */
public class SsoFilter extends ClientFilter {

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (SessionUtils.getUser(request) == null) {
            String ticket = request.getParameter(SsoConstant.TICKET);
            RpcUserDto rpcUser;
            if (ticket != null && (rpcUser = authenticationRpcService.validate(ticket)) != null) {
                // 存储sessionUser
                SessionUtils.setUser(request, new SessionUser(rpcUser.getId(), rpcUser.getAccount()));
                // 为去掉URL中token参数，再跳转一次当前Service
                response.sendRedirect(getRemoveTicketService(request));
            }
            else {
                redirectLogin(request, response);
            }
            return false;
        } 
        else {
            return true;
        }
	}

	/**
	 * 跳转至服务端登录
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
			String ssoLoginUrl = new StringBuilder().append(ssoServerUrl)
					.append("/login?service=").append(URLEncoder.encode(getService(request), "utf-8")).toString();

			response.sendRedirect(ssoLoginUrl);
		}
	}

    /**
     * 去除返回地址中的票据参数
     * 
     * @param request
     * @return
     */
	private String getRemoveTicketService(HttpServletRequest request) {
		String service = getService(request);
		return service.substring(0, service.indexOf(SsoConstant.TICKET) - 1);
	}

    /**
     * 获取当前请求地址
     * 
     * @param request
     * @return
     */
	private String getService(HttpServletRequest request) {
		return new StringBuilder().append(request.getRequestURL())
				.append(request.getQueryString() == null ? "" : "?" + request.getQueryString()).toString();
	}
}