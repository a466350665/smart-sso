package com.smart.sso.client.util;

import java.util.Optional;

import javax.servlet.http.HttpServletRequest;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.SessionUser;
import com.smart.sso.client.dto.SsoUser;

/**
 * 当前已登录用户Session工具
 * 
 * @author Joe
 */
public class SessionUtils {
    
    public static SessionUser getSessionUser(HttpServletRequest request) {
        return (SessionUser) request.getSession().getAttribute(SsoConstant.SESSION_USER);
    }
    
	public static SsoUser getUser(HttpServletRequest request) {
	    return Optional.ofNullable(getSessionUser(request)).map(u -> u.getUser()).orElse(null);
	}
	
	public static Integer getUserId(HttpServletRequest request) {
        return Optional.ofNullable(getUser(request)).map(u -> u.getId()).orElse(null);
    }

	public static void setSessionUser(HttpServletRequest request, SessionUser sessionUser) {
		request.getSession().setAttribute(SsoConstant.SESSION_USER, sessionUser);
	}

	public static void invalidate(HttpServletRequest request){
	    setSessionUser(request, null);
		request.getSession().invalidate();
	}
}