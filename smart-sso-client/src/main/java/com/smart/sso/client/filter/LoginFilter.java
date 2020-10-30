package com.smart.sso.client.filter;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLEncoder;
import java.text.MessageFormat;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;
import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.client.session.SessionAccessToken;
import com.smart.sso.client.session.SessionMappingStorage;
import com.smart.sso.client.session.SessionUser;
import com.smart.sso.client.session.SessionUtils;
import com.smart.sso.client.util.HttpUtils;

/**
 * 单点登录Filter
 * 
 * @author Joe
 */
public class LoginFilter extends ClientFilter {
    
    private final Logger logger = LoggerFactory.getLogger(getClass());
    
    private SessionMappingStorage sessionMappingStorage = LogoutFilter.getSessionMappingStorage();

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		SessionAccessToken accessToken = SessionUtils.getAccessToken(request);
		// 本地Session中已存在，且accessToken没过期或者refreshToken成功，直接返回
		if (accessToken != null && (!accessToken.isExpired()
				|| refreshToken(accessToken.getRefreshToken(), accessToken.getUser(), request))) {
			return true;
		}
		String code = request.getParameter(Oauth2Constant.AUTH_CODE);
		if (code != null) {
			// 获取accessToken
			getTokenAndUserInSession(code, request);
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
     * @return
     */
	private void getTokenAndUserInSession(String code, HttpServletRequest request) {
		String accessTokenUrl = MessageFormat.format(Oauth2Constant.ACCESS_TOKEN_URL, serverUrl, appId, appSecret,
				code);
		RpcAccessToken rpcAccessToken = getHttpJson(accessTokenUrl, RpcAccessToken.class);
		if (rpcAccessToken == null) {
			return;
		}
		String userinfoUrl = MessageFormat.format(Oauth2Constant.USERINFO_URL, serverUrl,
				rpcAccessToken.getAccessToken());
		RpcUser user = getHttpJson(userinfoUrl, RpcUser.class);
		if (user == null) {
			return;
		}
		SessionUser sessionUser = new SessionUser(user.getId(), user.getAccount());
		setTokenAndUserInSession(rpcAccessToken, sessionUser, request);
	}
	
	/**
     * 通过refreshToken参数调用http请求延长服务端session，并返回新的accessToken
     * 
     * @param refreshToken
     * @return
     */
	private boolean refreshToken(String refreshToken, SessionUser sessionUser, HttpServletRequest request) {
		String refreshTokenUrl = MessageFormat.format(Oauth2Constant.REFRESH_TOKEN_URL, serverUrl, appId, refreshToken);
		RpcAccessToken accessToken = getHttpJson(refreshTokenUrl, RpcAccessToken.class);
		if (accessToken == null) {
			return false;
		}
		setTokenAndUserInSession(accessToken, sessionUser, request);
		return true;
	}
	
	private <T> T getHttpJson(String url, Class<T> clazz) {
		String jsonStr = HttpUtils.get(url);
		if (jsonStr == null || jsonStr.isEmpty()) {
			logger.error("getHttpJson exception, return null. url:{}", url);
			return null;
		}
		Result<?> result = JSONObject.parseObject(jsonStr, Result.class);
		if (!result.isSuccess()) {
			logger.error("getHttpJson has error, url:{}, message:{}", url, result.getMessage());
			return null;
		}
		return JSONObject.parseObject(result.getData().toString(), clazz);
	}
    
	/**
	 * AccessToken存session
	 * 
	 * @param rpcAccessToken
	 * @param user
	 * @param request
	 */
	private void setTokenAndUserInSession(RpcAccessToken rpcAccessToken, SessionUser sessionUser, HttpServletRequest request) {
		SessionAccessToken sessionAccessToken = createSessionAccessToken(rpcAccessToken, sessionUser);
		
		// 本地session存accessToken
		SessionUtils.setAccessToken(request, sessionAccessToken);
		
		// 记录本地session和AccessToken映射
		recordSession(request, rpcAccessToken.getAccessToken());
	}
	
	private SessionAccessToken createSessionAccessToken(RpcAccessToken accessToken, SessionUser sessionUser) {
		// session存储accessToken失效时间
		long expirationTime = System.currentTimeMillis() + accessToken.getExpiresIn() * 1000;
		return new SessionAccessToken(accessToken.getAccessToken(), accessToken.getExpiresIn(),
				accessToken.getRefreshToken(), expirationTime, sessionUser);
	}
	
    private void recordSession(final HttpServletRequest request, String accessToken) {
        final HttpSession session = request.getSession();
        sessionMappingStorage.removeBySessionById(session.getId());
        sessionMappingStorage.addSessionById(accessToken, session);
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
			String loginUrl = MessageFormat.format(SsoConstant.LOGIN_URL, serverUrl, appId,
					URLEncoder.encode(getCurrentUrl(request), "utf-8"));
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