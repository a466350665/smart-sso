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
import java.util.UUID;

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

	public static AccessToken getAndRefresh(HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			return null;
		}
		return tokenStorage.getAndRefresh(st);
	}

	public static AccessToken get(HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			return null;
		}
		return tokenStorage.get(st);
	}

	public static Userinfo getUserinfo(HttpServletRequest request) {
		return Optional.ofNullable(get(request)).map(u -> u.getUserinfo()).orElse(null);
	}

	public static void setAccessToken(AccessToken at, HttpServletRequest request, HttpServletResponse response) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			st = "ST-" + UUID.randomUUID().toString().replaceAll("-", "");
			// 写入cookie
			CookieUtils.addCookie(ClientConstant.COOKIE_ST, st, "/", request, response);
		}
		tokenStorage.create(st, at);
	}

	private static String getCookieServiceTicket(HttpServletRequest request) {
		return CookieUtils.getCookie(request, ClientConstant.COOKIE_ST);
	}
}