package com.smart.sso.client.util;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

	private static final Logger logger = LoggerFactory.getLogger(TokenUtils.class);

	private static TokenStorage tokenStorage;

	public static void setTokenStorage(TokenStorage ts) {
		tokenStorage = ts;
	}

	/**
	 * 获取accessToken
	 * 1.如果获取accessToken没过期，直接返回
	 * 2.如果获取accessToken已过期，refreshToken没过期，使用refresh接口刷新再返回
	 *
	 * @param properties
	 * @param request
	 * @return
	 */
	public static AccessToken getAndRefresh(ClientProperties properties, HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			return null;
		}
		TokenWrapper wrapper = tokenStorage.get(st);
		if (wrapper == null) {
			return null;
		}
		// accessToken没过期直接返回
		if (!wrapper.checkExpired()) {
			return wrapper.getObject();
		}
		// accessToken已过期，refreshToken没过期，使用refresh接口刷新
		if (!wrapper.checkRefreshExpired()) {
			AccessToken at = Oauth2Utils.getHttpRefreshToken(properties, wrapper.getObject().getRefreshToken());
			if (at != null) {
				tokenStorage.remove(st);
				tokenStorage.create(st, new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn()));
				return at;
			}
		}
		return null;
	}

	public static AccessToken get(HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			return null;
		}
		TokenWrapper wrapper = tokenStorage.get(st);
		if (wrapper == null) {
			return null;
		}
		// accessToken没过期直接返回
		if (!wrapper.checkExpired()) {
			return wrapper.getObject();
		}
		return null;
	}

	public static Userinfo getUserinfo(HttpServletRequest request) {
		return Optional.ofNullable(get(request)).map(u -> u.getUserinfo()).orElse(null);
	}

	public static void set(AccessToken at, HttpServletRequest request, HttpServletResponse response) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			// 写入cookie
			CookieUtils.addCookie(ClientConstant.COOKIE_ST, at.getAccessToken(), "/", request, response);
		}
		tokenStorage.create(st, new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn()));
	}

	private static String getCookieServiceTicket(HttpServletRequest request) {
		return CookieUtils.getCookie(request, ClientConstant.COOKIE_ST);
	}
}