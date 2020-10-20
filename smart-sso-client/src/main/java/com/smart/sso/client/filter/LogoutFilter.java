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
        String token = request.getParameter(SsoConstant.TICKET_PARAMETER_NAME);
        if (token != null) {
            recordSession(request, token);
            return true;
        }

        token = request.getParameter(SsoConstant.LOGOUT_PARAMETER_NAME);
        if (token != null) {
            destroySession(token);
            return false;
        }

        return true;
    }

    private void recordSession(final HttpServletRequest request, String token) {
        final HttpSession session = request.getSession();
        sessionMappingStorage.removeBySessionById(session.getId());
        sessionMappingStorage.addSessionById(token, session);
    }

    private void destroySession(String token) {
        final HttpSession session = sessionMappingStorage.removeSessionByMappingId(token);
        if (session != null) {
            session.invalidate();
        }
    }

    public static SessionMappingStorage getSessionMappingStorage() {
        return sessionMappingStorage;
    }
}