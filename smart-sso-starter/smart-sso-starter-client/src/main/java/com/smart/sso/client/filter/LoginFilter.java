package com.smart.sso.client.filter;

import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.entity.ClientAccessToken;
import com.smart.sso.client.entity.Result;
import com.smart.sso.client.util.JsonUtils;
import com.smart.sso.client.util.Oauth2Utils;
import com.smart.sso.client.util.TokenUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLEncoder;

/**
 * 单点登录Filter
 * 
 * @author Joe
 */
public class LoginFilter extends ClientFilter {
	
	private final Logger logger = LoggerFactory.getLogger(getClass());
    
	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		ClientAccessToken accessToken = TokenUtils.getAccessToken(request, response);
		// 本地已存在accessToken，直接返回
		if (accessToken != null) {
			return true;
		}
		String code = request.getParameter(Oauth2Constant.AUTH_CODE);
		// 携带授权码请求
		if (code != null) {
			// 获取accessToken
			getAccessToken(code, request);
			// 为去除URL中授权码参数，再跳转一次当前地址
			redirectLocalRemoveCode(request, response);
		}
		else {
			redirectLogin(request, response);
		}
		return false;
	}

	/**
	 * 获取accessToken和用户信息存储到Token管理器
	 *
	 * @param code
	 * @param request
	 */
	private void getAccessToken(String code, HttpServletRequest request) {
		Result<ClientAccessToken> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getAppId(),
				properties.getAppSecret(), code);
		if (!result.isSuccess()) {
			logger.error("getAccessToken has error, message:{}", result.getMessage());
			return;
		}
		TokenUtils.setAccessToken(result.getData(), request);
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
			responseJson(response, ClientConstant.NO_LOGIN, "未登录或已超时");
		}
		else {
			String loginUrl = new StringBuilder().append(properties.getServerUrl()).append(ClientConstant.LOGIN_URL).append("?")
					.append(Oauth2Constant.APP_ID).append("=").append(properties.getAppId()).append("&")
					.append(ClientConstant.REDIRECT_URI).append("=")
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
        writer.write(JsonUtils.toJSONString(Result.create(code, message)));
        writer.flush();
        writer.close();
    }
}