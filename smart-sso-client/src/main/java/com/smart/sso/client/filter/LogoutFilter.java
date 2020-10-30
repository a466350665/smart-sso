package com.smart.sso.client.filter;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.session.LocalSessionMappingStorage;
import com.smart.sso.client.session.SessionMappingStorage;

/**
 * 单点登出Filter
 * 
 * @author Joe
 */
public class LogoutFilter extends ClientFilter {

    private static SessionMappingStorage sessionMappingStorage = new LocalSessionMappingStorage();
    
    @Override
    public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String accessToken = request.getParameter(SsoConstant.LOGOUT_PARAMETER_NAME);
        if (accessToken != null) {
            destroySession(accessToken);
            return false;
        }
        return true;
    }

    private void destroySession(String accessToken) {
        final HttpSession session = sessionMappingStorage.removeSessionByMappingId(accessToken);
        if (session != null) {
            session.invalidate();
        }
    }

    public static SessionMappingStorage getSessionMappingStorage() {
        return sessionMappingStorage;
    }
}