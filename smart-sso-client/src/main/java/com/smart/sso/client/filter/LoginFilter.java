package com.smart.sso.client.filter;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.Result;
import com.smart.sso.client.dto.SessionUser;
import com.smart.sso.client.dto.AccessToken;
import com.smart.sso.client.util.HttpUtils;
import com.smart.sso.client.util.SessionUtils;

/**
 * 单点登录Filter
 * 
 * @author Joe
 */
public class LoginFilter extends ClientFilter {
    
    private final Logger logger = LoggerFactory.getLogger(getClass());

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		SessionUser sessionUser;
		if ((sessionUser = SessionUtils.getSessionUser(request)) != null && refreshServerSession(sessionUser)) {
			return true;
		}
		String ticket = request.getParameter(SsoConstant.TICKET_PARAMETER_NAME);
		if (ticket != null) {
			AccessToken tokenDto = validateTicket(ticket);
			if (tokenDto != null) {
				// 存储sessionUser
				setUserInSession(tokenDto, request);
			}
			// 为去掉URL中token参数，再跳转一次当前Service
			redirectRemoveTicketService(request, response);
		}
		else {
			redirectLogin(request, response);
		}
		return false;
	}
	
    /**
     * 校验ticket有效性
     * 
     * @param ticket
     * @return
     */
    private AccessToken validateTicket(String ticket) {
        String tokenStr = HttpUtils
            .get(new StringBuilder().append(ssoServerUrl).append("/cas/validate?ticket=").append(ticket).toString());
        if (tokenStr == null || tokenStr.isEmpty()) {
            logger.error("ticket validate exception, return null. ticket:{}", ticket);
            return null;
        }
        Result<AccessToken> result = JSONObject.parseObject(tokenStr, new TypeReference<Result<AccessToken>>() {});
        if (!result.isSuccess()) {
            logger.error("ticket validate exception, ticket:{}, message:{}", ticket, result.getMessage());
            return null;
        }
        return result.getData();
    }
    
	/**
	 * 用户信息存session
	 * 
	 * @param tokenDto
	 * @param request
	 */
	private void setUserInSession(AccessToken tokenDto, HttpServletRequest request) {
		SessionUser sessionUser = new SessionUser(System.currentTimeMillis() + tokenDto.getTimeout() * 1000,
				tokenDto.getTimeout(), tokenDto.getRefreshToken(), tokenDto.getUser());
		// 存储sessionUser
		SessionUtils.setSessionUser(request, sessionUser);
	}
    
    /**
     * 当服务端回传给客户端的时效过期，发起http请求延长服务端session时效
     * 
     * @param sessionUser
     */
	private boolean refreshServerSession(SessionUser sessionUser) {
		if (System.currentTimeMillis() > sessionUser.getRefreshTime()) {
			String refreshToken = invokeHttpRefresh(sessionUser.getRefreshToken());
			if (refreshToken == null || refreshToken.isEmpty()) {
				return false;
			}
			sessionUser.setRefreshTime(System.currentTimeMillis() + sessionUser.getTimeout() * 1000);
			sessionUser.setRefreshToken(refreshToken);
		}
		return true;
	}
    
    /**
     * 通过refreshToken参数调用http请求延长服务端session，并返回新的refreshToken
     * 
     * @param refreshToken
     * @return
     */
    private String invokeHttpRefresh(String refreshToken) {
		String newRefreshToken = HttpUtils.get(new StringBuilder().append(ssoServerUrl)
				.append("/cas/refresh?refreshToken=").append(refreshToken).toString());
		if (newRefreshToken == null || newRefreshToken.isEmpty()) {
			logger.error("refresh exception, return null. refreshToken:{}", refreshToken);
			return null;
		}
		Result<String> result = JSONObject.parseObject(newRefreshToken, new TypeReference<Result<String>>() {});
		if (!result.isSuccess()) {
			logger.error("refresh exception, refreshToken:{}, message:{}", refreshToken, result.getMessage());
			return null;
		}
		return result.getData();
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
     * @throws IOException
     */
    private void redirectRemoveTicketService(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String service = getService(request);
        service = service.substring(0, service.indexOf(SsoConstant.TICKET_PARAMETER_NAME) - 1);
        response.sendRedirect(service);
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
	
	protected boolean isAjaxRequest(HttpServletRequest request) {
        String requestedWith = request.getHeader("X-Requested-With");
        return requestedWith != null ? "XMLHttpRequest".equals(requestedWith) : false;
    }
    
    protected void responseJson(HttpServletResponse response, int code, String message) throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(200);
        PrintWriter writer = response.getWriter();
        writer.write(JSON.toJSONString(Result.create(code, message)));
        writer.flush();
        writer.close();
    }
}