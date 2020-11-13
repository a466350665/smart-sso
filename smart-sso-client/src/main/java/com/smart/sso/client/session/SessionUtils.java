package com.smart.sso.client.session;

import java.util.Optional;

import javax.servlet.http.HttpServletRequest;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.rpc.SsoUser;

/**
 * 当前已登录用户Session工具
 * 
 * @author Joe
 */
public class SessionUtils {
    
    public static SessionAccessToken getAccessToken(HttpServletRequest request) {
        return (SessionAccessToken) request.getSession().getAttribute(SsoConstant.SESSION_ACCESS_TOKEN);
    }
    
	public static SsoUser getUser(HttpServletRequest request) {
	    return Optional.ofNullable(getAccessToken(request)).map(u -> u.getUser()).orElse(null);
	}
	
	public static Integer getUserId(HttpServletRequest request) {
        return Optional.ofNullable(getUser(request)).map(u -> u.getId()).orElse(null);
    }

	public static void setAccessToken(HttpServletRequest request, SessionAccessToken sessionUser) {
		request.getSession().setAttribute(SsoConstant.SESSION_ACCESS_TOKEN, sessionUser);
	}

	public static void invalidate(HttpServletRequest request){
		setAccessToken(request, null);
		request.getSession().invalidate();
	}
}