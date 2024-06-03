package com.smart.sso.client.filter;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSON;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;
import com.smart.sso.client.session.SessionAccessToken;
import com.smart.sso.client.util.Oauth2Utils;
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
		SessionAccessToken sessionAccessToken = SessionUtils.getAccessToken(request);
		// 本地Session中已存在，且accessToken没过期或者refreshToken成功，直接返回
		if (sessionAccessToken != null && (!sessionAccessToken.isExpired()
				|| refreshToken(sessionAccessToken.getRefreshToken(), request))) {
			return true;
		}
		String code = request.getParameter(Oauth2Constant.AUTH_CODE);
		if (code != null) {
			// 获取accessToken
			getAccessToken(code, request);
			// 为去掉URL中授权码参数，再跳转一次当前地址
			redirectLocalRemoveCode(request, response);
		}
		else {
			redirectLogin(request, response);
		}
		return false;
	}
	
    /**
     * 获取accessToken和用户信息存session
     * 
	 * @param code
	 * @param request
	 */
	private void getAccessToken(String code, HttpServletRequest request) {
		Result<RpcAccessToken> result = Oauth2Utils.getAccessToken(getServerUrl(), getAppId(),
				getAppSecret(), code);
		if (!result.isSuccess()) {
			logger.error("getAccessToken has error, message:{}", result.getMessage());
			return;
		}
		setAccessTokenInSession(result.getData(), request);
	}
	
	/**
     * 通过refreshToken参数调用http请求延长服务端session，并返回新的accessToken
     * 
	 * @param refreshToken
	 * @param request
	 * @return
	 */
	protected boolean refreshToken(String refreshToken, HttpServletRequest request) {
		Result<RpcAccessToken> result = Oauth2Utils.refreshToken(getServerUrl(), getAppId(), refreshToken);
		if (!result.isSuccess()) {
			logger.error("refreshToken has error, message:{}", result.getMessage());
			return false;
		}
		return setAccessTokenInSession(result.getData(), request);
	}
	
	private boolean setAccessTokenInSession(RpcAccessToken rpcAccessToken, HttpServletRequest request) {
		if (rpcAccessToken == null) {
			return false;
		}
		// 记录accessToken到本地session
		SessionUtils.setAccessToken(request, rpcAccessToken);
		
		// 记录本地session和accessToken映射
		recordSession(request, rpcAccessToken.getAccessToken());
		return true;
	}
	
    private void recordSession(final HttpServletRequest request, String accessToken) {
        final HttpSession session = request.getSession();
        getSessionMappingStorage().removeBySessionById(session.getId());
        getSessionMappingStorage().addSessionById(accessToken, session);
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
			String loginUrl = new StringBuilder().append(getServerUrl()).append(SsoConstant.LOGIN_URL).append("?")
					.append(Oauth2Constant.APP_ID).append("=").append(getAppId()).append("&")
					.append(SsoConstant.REDIRECT_URI).append("=")
					.append(URLEncoder.encode(getCurrentUrl(request), "utf-8")).toString();
			response.sendRedirect(loginUrl);
		}
	}

    /**
     * 去除返回地址中的票据参数
     * 
     * @param request
     * @return
     * @throws IOException
     */
    private void redirectLocalRemoveCode(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String currentUrl = getCurrentUrl(request);
        currentUrl = currentUrl.substring(0, currentUrl.indexOf(Oauth2Constant.AUTH_CODE) - 1);
        response.sendRedirect(currentUrl);
    }
	
    /**
     * 获取当前请求地址
     * 
     * @param request
     * @return
     */
	private String getCurrentUrl(HttpServletRequest request) {
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