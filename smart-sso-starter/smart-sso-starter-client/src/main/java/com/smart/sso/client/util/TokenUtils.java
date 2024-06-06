package com.smart.sso.client.util;

import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.entity.ClientAccessToken;
import com.smart.sso.client.entity.ClientUser;
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

	public static ClientAccessToken getAccessToken(HttpServletRequest request, HttpServletResponse response) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			// 创建st并写入cookie
			createServiceTicketInCookie(request, response);
			return null;
		}
		return tokenStorage.getAndRefresh(st);
    }

	public static ClientAccessToken getAccessToken(HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (StringUtils.isEmpty(st)) {
			return null;
		}
		return tokenStorage.getAndRefresh(st);
	}
    
	public static ClientUser getUser(HttpServletRequest request) {
	    return Optional.ofNullable(getAccessToken(request)).map(u -> u.getUser()).orElse(null);
	}
	
	public static Integer getUserId(HttpServletRequest request) {
        return Optional.ofNullable(getUser(request)).map(u -> u.getId()).orElse(null);
    }

	public static void setAccessToken(ClientAccessToken accessToken, HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (!StringUtils.isEmpty(st)) {
			tokenStorage.create(st, accessToken);
		}
	}

	public static void invalidate(HttpServletRequest request) {
		String st = getCookieServiceTicket(request);
		// cookie中没有
		if (!StringUtils.isEmpty(st)) {
			tokenStorage.removeByServiceTicket(st);
		}
	}

	private static String createServiceTicketInCookie(HttpServletRequest request, HttpServletResponse response){
		String st = "ST-" + UUID.randomUUID().toString().replaceAll("-", "");
		// ST存cookie
		CookieUtils.addCookie(ClientConstant.COOKIE_SERVICE_TICKET, st, "/", request, response);
		return st;
	}

	private static String getCookieServiceTicket(HttpServletRequest request) {
		return CookieUtils.getCookie(request, ClientConstant.COOKIE_SERVICE_TICKET);
	}
}