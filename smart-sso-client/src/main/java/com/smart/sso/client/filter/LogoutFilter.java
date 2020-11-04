package com.smart.sso.client.filter;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.smart.sso.client.constant.SsoConstant;

/**
 * 单点登出Filter
 * 
 * @author Joe
 */
public class LogoutFilter extends ClientFilter {

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
        final HttpSession session = getSessionMappingStorage().removeSessionByMappingId(accessToken);
        if (session != null) {
            session.invalidate();
        }
    }
}