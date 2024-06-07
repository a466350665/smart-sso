package com.smart.sso.client.util;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.token.TokenStorage;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Optional;

/**
 * Token工具
 * 
 * @author Joe
 */
public class TokenUtils {

	private static TokenStorage tokenStorage;

	public static void setTokenStorage(TokenStorage ts) {
		tokenStorage = ts;
	}

	public static AccessToken getAccessToken(HttpServletRequest request) {
		String accessToken = getCookieAccessToken(request);
		// cookie中没有
		if (StringUtils.isEmpty(accessToken)) {
			return null;
		}
		return tokenStorage.getAndRefresh(accessToken);
	}
    
	public static Userinfo getUserinfo(HttpServletRequest request) {
	    return Optional.ofNullable(getAccessToken(request)).map(u -> u.getUserinfo()).orElse(null);
	}
	
	public static Integer getUserId(HttpServletRequest request) {
        return Optional.ofNullable(getUserinfo(request)).map(u -> u.getId()).orElse(null);
    }

	public static void setAccessToken(AccessToken at, HttpServletRequest request, HttpServletResponse response) {
		// 将accessToken写入cookie
		CookieUtils.addCookie(ClientConstant.COOKIE_ACCESS_TOKEN, at.getAccessToken(), "/", request, response);
		tokenStorage.create(at);
	}

	public static void invalidate(HttpServletRequest request) {
		String accessToken = getCookieAccessToken(request);
		// cookie中没有
		if (!StringUtils.isEmpty(accessToken)) {
			tokenStorage.remove(accessToken);
		}
	}

	private static String getCookieAccessToken(HttpServletRequest request) {
		return CookieUtils.getCookie(request, ClientConstant.COOKIE_ACCESS_TOKEN);
	}
}